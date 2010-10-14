(in-package :kilns)

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
          (name obj) (process obj)
          (eql (continuation obj) null-process) (continuation obj)))

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


;;; whenever a process is added to a network kell, we share the process across all hosts
;;; inside that kell it gets added to each of their event queues. So, the network kells
;;; should basically be in sync.
;;; 
;;; When a process in a network kell is matched, its removal notice is sent to all hosts.
;;; Each has to reply that it has been removed before the reaction actually occurs.
;;; 
;;; If there's a timeout, the host that times out is passivated, and a notice is sent to
;;; all hosts to passivate that host.

(defvar *host-definitions* ())
(defvar *local-host*)
(defvar *local-port*)

(defclass host-kell (kell)
  ((hostname :initarg :hostname :reader hostname)
   (port :initarg :port :reader port)
   (socket :initform nil))
  (:documentation "This is a kell that represents the processes running on a particular
                   host. It is opaque from the current machine."))

(defmethod print-object ((obj host-kell) stream)
  (format stream "[~a <~a:~d>~:[ ~a~;~]]"
          (name obj) (hostname obj) (port obj)
          (eql (continuation obj) null-process) (continuation obj)))

(defmethod socket ((kell host-kell))
  (or (slot-value kell 'socket)
      (setf (slot-value kell 'socket)
            (make-socket :address-family :internet
                     :type :stream
                     :connect
                     :local-host *local-host*
                     :local-port *local-port*
                     :remote-host (hostname kell)
                     :remote-port (port kell)))))

(defmethod add-process ((process kell) (kell network-kell))
  "When adding a kell to a network kell, it can be either the kell representing the
   current host, a kell representing a different host, or another level of network kell.
   This determines which one it should be and adds an appropriate subkell."
  (let ((new-kell (cond ((string-equal (name process) *local-kell*)
                         process)
                        ((assoc (list (name process)) *host-definitions*
                                :test #'equal)
                         (destructuring-bind ((name) host port)
                                             (assoc (list (name process))
                                                    *host-definitions*
                                                    :test #'equal)
                           (make-instance 'host-kell :name name :hostname host :port port)))
                        (t (change-class process 'network-kell)
                           process))))
    (setf (process kell)
          (compose-processes new-kell (process kell)))
    (activate-process new-kell kell)))

(defmethod activate-process (process (kell host-kell))
  (declare (ignore process))
  (error "No processes can exist inside a host kell."))

;;; FIXME: need to define a LOCAL-KELL, so that a network kell can't be confused with it
;;(defmethod activate-process ((process network-kell) (kell kell))
;;  (error "A network kell can not exist inside a local kell."))
;;(defmethod activate-process ((process host-kell) (kell kell))
;;  (error "A host kell can not exist inside a local kell."))
  
(defmethod add-process (process (kell host-kell))
  (declare (ignore process))
  (error "No processes can exist inside a host kell."))
;;(defmethod add-process ((process network-kell) (kell kell))
;;  (error "A network kell can not exist inside a local kell."))
;;(defmethod add-process ((process host-kell) (kell kell))
;;  (error "A host kell can not exist inside a local kell."))

(defun defhost (kell-path hostname port)
  "A defhost also implies that all containing kells are network kells."
  ;; FIXME: surrounding-kell-path should be the path from the outermost kell to the
  ;;        kell the file is loaded in
  (let ((surrounding-kell-path))
    (push (list (append surrounding-kell-path
                        (if (consp kell-path) kell-path (list kell-path)))
                hostname
                port)
          *host-definitions*))
  null-process)

(defun send-process (process kell)
  (write-binary-record process *standard-output*))



;;; Adding-processes to network kells
(defmethod activate-process ((process process) (kell network-kell))
    (setf (parent process) kell)
    (mapc #'broadcast-event (collect-channel-names process kell)))

(defun broadcast-event (item)
  ;;; FIXME: implement
  (values))

(defun receive-broadcast-event (item)
  ;;; FIXME: I think we need to do something to convert the kellpath to a kell
  (push-event item))

(defun select-matching-pattern (patterns)
  (catch 'match
    (mapc (lambda (trigger)
            (handler-case
                (destructuring-bind (processes substitutions)
                    (match (pattern trigger) (parent trigger))
                  (throw 'match (list trigger processes substitutions)))
              (unification-failure ())))
          patterns)))

(defmethod really-match-on ((process message) (kell network-kell))
  "Find all triggers that could match – up, down, or local."
  (let ((name (name process)))
    (select-matching-pattern
     (remove-duplicates (append (gethash name (local-patterns kell))
                                (gethash name (down-patterns (parent kell)))
                                (mapcan (lambda (subkell)
                                          (gethash name
                                                   (up-patterns subkell)))
                                        (subkells kell)))))))
(defmethod really-match-on ((process kell) (kell network-kell))
  "Find all triggers that could match."
  (select-matching-pattern (gethash (name process) (kell-patterns kell))))
(defmethod really-match-on ((process trigger) (kell network-kell))
  "Just match on the new trigger."
  (select-matching-pattern (list process)))

;;; IOLib stuff

(defun send-process (process dest-kell)
  (send-to (socket dest-kell)
           (map '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
                #'char-code
                (format nil "~a" process))))

(defun receive-process (source-kell)
  (read-from-string (map 'string #'code-char (receive-from (socket source-kell)))))
