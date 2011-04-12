(in-package :kilns)

;; Came up with the default port number by mapping the letters i–s to 0–10, then
;; spelling “kilns” with those numbers:
;; (k -> 2), (i -> 0), (l -> 3), (n -> 5), (s -> 10)
(defvar *base-port* 20360)

;;; Protocol format – use kilns reader
;;; {keep-alive}
;;; {handshake (par {path {from {top {kell}}}}
;;;                 {version "0.0.1"})}

(defun handle-request (client)
  (unwind-protect
       (multiple-value-bind (who remote-port) (sockets:remote-name client)
         (handler-case
           (let* ((*readtable* *kilns-readtable*)
                  (*package* (find-package :kilns-user))
                  (request (eval (read client))))
             (case (name request)
               (kilns-user::keep-alive
                (write (eval (read-from-string "{keep-alive}"))
                       :stream client))
               (kilns-user::handshake
                (let* ((subst (match-local (eval (read-from-string "{handshake (par {path ?path} {version ?version})}"))
                                           request))
                       (version (find-variable-value (intern "?version")
                                                     subst nil)))
                  (if (equal version "0.0.1")
                      (write (eval (read-from-string "{handshake (par {path {path {to {local {kell}}}}} {version \"0.0.1\"})}"))
                             :stream client)
                      (write (eval (read-from-string "{handshake {error \"incompatible protocol\"}}"))
                             :stream client)))))
             
             (finish-output client))
           (sockets:socket-connection-reset-error ()
             (format t "Client reset connection!~%"))
           (iolib.streams:hangup () (format t "Client closed conection!~%"))))
    (close client)))

(defun dispatch-request (socket)
  (let ((client (sockets:accept-connection socket :wait t)))
    (when client
      (make-thread (lambda () (handle-request client))
                   :name "request-handler"))))

(defun start-kilns-listener (&optional port)
  (make-thread (lambda ()
                 (handler-case
                     (sockets:with-open-socket
                         (socket :connect :passive
                                 :address-family :internet
                                 :type :stream
                                 :external-format '(:utf-8 :eol-style :lf)
                                 :ipv6 nil
                                 :local-host sockets:+ipv4-unspecified+
                                 :local-port (or port *base-port*)
                                 :reuse-address t)
                       (sockets:listen-on socket :backlog 5)
                       (loop while socket
                          do (dispatch-request socket)))
                   (sockets:socket-address-in-use-error ()
                     ;; FIXME: we should try another port
                     (format t "Bind: Address already in use, forgot :reuse-addr t?"))))
               :name "kilns network listener"))

;;; This file implements the behavior of distributed kells. They need to be
;;; modeled on each instance, and behavior that crosses boundaries needs to be
;;; initiated by the host that most causes the reaction – depending on the level
;;; of the reaction, it might have to update the state for multiple hosts.

(defclass network-kell (kell)
  ((multicast-address
    :documentation "Each network kell has a multicast group address. Every host
                    in that kell needs to both be a member of that group _and_ a
                    sender to that group, all the way up to the top kell. The
                    problem is that there are 256 multicast addresses to be used
                    within each subnet. These need to be divided intelligently
                    among the layers to maximize efficiency (if each smaller
                    subnet allocates a different set of multicast addresses,
                    then that encroaches on the set available for enclosing
                    subnets). Also, multicasting outside of subnets may be
                    problematic as well, since the space is chopped up and
                    largely allocated. IPv6 might offer some help.")
   (uses-ssl)
   (uses-encryption)))

(defmethod print-object ((obj network-kell) stream)
  "Adds a little indicator to the kell to mark it as a network kell."
  (format stream "[<@>~a ~a~:[ ~a~;~]]"
          (name obj) (state obj)
          (eql (continuation obj) null) (continuation obj)))

;;; A multicast message is sent whenever 'match-on' is triggered on a network
;;; kell. Unlike local matches, once the message is sent, the host continues to
;;; process other events while it waits for a response. Network events that have
;;; started processing have a higher priority than local events, as handling
;;; them quickly can result in less work done on the network (since every host
;;; in the multicast group starts working on a match when it receives a
;;; request).
;;; 
;;; ======= THIS PARAGRAPH MAY BE A BAD IDEA =======
;;; The hosts that received the multicast (including the sender) respond with a
;;; unicast request containing any messages they have that match individual
;;; messages in the pattern. The recipient sends out another multicast when it
;;; has at least one message for each in the pattern, which is to stop other
;;; hosts from building any more responses.
;;; 
;;; The originator then sends out unicast addresses to each host that it is
;;; using messages from, to tell them that they were used in the match. The
;;; recipient then responds saying "great, they're still available (or not)",
;;; and the originator sends out a final multicast(?) confirming that the match
;;; has been completed, or that it has been rolled back, in which case it goes
;;; in the queue to be tried again.


;;; whenever a process is added to a network kell, we share the process across
;;; all hosts inside that kell it gets added to each of their event queues. So,
;;; the network kells should basically be in sync.
;;; 
;;; When a process in a network kell is matched, its removal notice is sent to
;;; all hosts. Each has to reply that it has been removed before the reaction
;;; actually occurs.
;;; 
;;; If there's a timeout, the host that times out is passivated, and a notice is
;;; sent to all hosts to passivate that host.

(defvar *top-kell*)
(defvar *local-kell*)
(defvar *host-definitions* ())
(defvar *local-host*)
(defvar *local-port*)
(defvar *real-kell*)

(defclass host-kell (kell)
  ((hostname :initarg :hostname :reader hostname)
   (port :initarg :port :reader port)
   (socket :initform nil))
  (:documentation "This is a kell that represents the processes running on a
                   particular host. It is opaque from the current machine."))

(defmethod print-object ((obj host-kell) stream)
  (format stream "[~a <~a:~d>~:[ ~a~;~]]"
          (name obj) (hostname obj) (port obj)
          (eql (continuation obj) null) (continuation obj)))

(defgeneric socket (kell)
  (:method ((kell host-kell))
    (or (slot-value kell 'socket)
        (setf (slot-value kell 'socket)
              (sockets:make-socket :address-family :internet
                                   :type :stream
                                   :connect :active
                                   :remote-host (hostname kell)
                                   :remote-port (port kell)
                                   :keepalive t)))))

(defmethod add-process ((process kell) (kell network-kell) &optional watchp)
  "When adding a kell to a network kell, it can be either the kell representing
   the current host, a kell representing a different host, or another level of
   network kell. This determines which one it should be and adds an appropriate
   subkell."
  (declare (ignore watchp))
  (let ((new-kell (cond ((string-equal (name process) *local-kell*)
                         (ccl::def-standard-initial-binding *real-kell* process)
                         (setf *real-kell* process))
                        ((assoc (list (name process)) *host-definitions*
                                :test #'equal)
                         (destructuring-bind ((name) host port)
                                             (assoc (list (name process))
                                                    *host-definitions*
                                                    :test #'equal)
                           (make-instance 'host-kell
                             :name name :hostname host :port port)))
                        (t (change-class process 'network-kell)
                           process))))
    (setf (state kell) (compose new-kell (state kell)))
    (activate-process new-kell kell)))

(defmethod activate-process (process (kell host-kell))
  (declare (ignore process))
  (error "No processes can exist inside a host kell."))

;;; FIXME: need to define a LOCAL-KELL, so that a network kell is distinct
;;(defmethod activate-process ((process network-kell) (kell kell))
;;  (error "A network kell can not exist inside a local kell."))
;;(defmethod activate-process ((process host-kell) (kell kell))
;;  (error "A host kell can not exist inside a local kell."))
  
(defmethod add-process (process (kell host-kell) &optional watchp)
  (declare (ignore process watchp))
  (error "No processes can exist inside a host kell."))
;;(defmethod add-process ((process network-kell) (kell kell))
;;  (error "A network kell can not exist inside a local kell."))
;;(defmethod add-process ((process host-kell) (kell kell))
;;  (error "A host kell can not exist inside a local kell."))

(defun defhost (kell-path hostname port)
  "A defhost also implies that all containing kells are network kells."
  (if (string-equal kell-path *local-kell*)
    (listen-for-processes port)
    ;; FIXME: surrounding-kell-path should be the path from the outermost kell
    ;;        to the kell the file is loaded in
    (let ((surrounding-kell-path))
      (push (list (append surrounding-kell-path
                          (if (consp kell-path) kell-path (list kell-path)))
                  hostname
                  port)
            *host-definitions*)))
  null)

;;; Adding-processes to network kells
(defmethod add-process ((process process) (kell network-kell) &optional watchp)
  (declare (ignore watchp))
  (mapc (lambda (sk)
          ;; FIXME: should probably allow kells here
          (when (and (not (typep process 'kell))
                     (not (typep process 'trigger))
                     (typep sk 'host-kell))
            (send-process process sk)))
        (subkells kell))
  (call-next-method))

(defun broadcast-event (item)
  ;;; FIXME: implement
  (declare (ignore item))
  (values))

(defun receive-broadcast-event (item)
  ;;; FIXME: I think we need to do something to convert the kellpath to a kell
  (push-event item))

(defmethod match-on ((process message) (kell network-kell))
  "Find all triggers that could match – up, down, or local."
  (select-matching-pattern (find-triggers-matching-message (name process) kell)
                           process))
(defmethod match-on ((process kell) (kell network-kell))
  "Find all triggers that could match."
  (select-matching-pattern (gethash (name process) (kell-patterns kell))
                           process))
(defmethod match-on ((process trigger) (kell network-kell))
  "Just match on the new trigger."
  (select-matching-pattern (list process) process))

;;; IOLib stuff

(defun listen-for-processes (port)
  (make-thread (lambda ()
                 (let ((socket (sockets:make-socket :address-family :internet
                                                    :type :stream
                                                    :connect :passive)))
                   (sockets:bind-address socket sockets:+ipv4-unspecified+
                                         :port port :reuse-addr t)
                   (sockets:listen-on socket)
                   (loop for client = (sockets:accept-connection socket :wait t)
                     do (loop do
                          (handler-case (let ((process (eval (read client))))
                                          (add-process process
                                                       (the kell (parent *real-kell*))))
                            (end-of-file () (return))
                            (kiln-error (c) (handle-error c))))
                     (close client))
                   (close socket)
                   (finish-output)))
               :name "network-kell"))

(defun send-process (process dest-kell)
  (sockets:send-to (socket dest-kell)
                   (map '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
                        #'char-code
                        (format nil "~s" process))))
