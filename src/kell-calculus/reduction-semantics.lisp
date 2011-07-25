#+xcvb (module (:depends-on ("utilities" "syntax")))
(in-package :kell-calculus)

(defmethod structurally-congruent-p ((left pattern) (right pattern))
  (and (set= (free-names left) (free-names right))
       (set= (channel-names left) (channel-names right))
       (set= (union (bound-names left) (bound-variables left))
             (union (bound-names right) (bound-variables right)))))

(defgeneric apply-restriction (local-name global-name process)
  (:documentation "Replaces all instances of a restricted name with a globally-
                   unique name.")
  (:method :around (local-name global-name process)
    (declare (ignorable global-name)) ; FIXME: not really, but CCL complains
    (if (find local-name (free-names process))
        (call-next-method)
        process))
  (:method (local-name global-name process)
    (declare (ignore local-name global-name))
    process)
  (:method (local-name global-name (process message))
    (message (if (eql (name process) local-name) global-name (name process))
             (apply-restriction local-name global-name (argument process))
             (apply-restriction local-name global-name (continuation process))))
  (:method (local-name global-name (process kell))
    (kell (if (eql (name process) local-name) global-name (name process))
          (apply-restriction local-name global-name (state process))
          (apply-restriction local-name global-name (continuation process))))
  (:method (local-name global-name (process parallel-composition))
    (map-process (lambda (proc) (apply-restriction local-name global-name proc))
                 process))
  (:method (local-name global-name (process pattern))
    (make-instance 'pattern
                   :local-message-pattern
                   (mapcar (lambda (message)
                             (apply-restriction local-name global-name message))
                           (local-message-pattern process))
                   :down-message-pattern
                   (mapcar (lambda (message)
                             (apply-restriction local-name global-name message))
                           (down-message-pattern process))
                   :up-message-pattern
                   (mapcar (lambda (message)
                             (apply-restriction local-name global-name message))
                           (up-message-pattern process))
                   :kell-message-pattern
                   (mapcar (lambda (message)
                             (apply-restriction local-name global-name message))
                           (kell-message-pattern process))))
  (:method (local-name global-name (process restriction-abstraction))
    (make-instance (class-of process)
                   :names (names process)
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))))
  (:method (local-name global-name (process pattern-abstraction))
    (make-instance (class-of process)
                   :pattern (apply-restriction local-name
                                               global-name
                                               (pattern process))
                   :process (apply-restriction local-name
                                               global-name
                                               (process process))))
  (:method (local-name global-name (process kell-abstraction))
    (make-instance (class-of process)
                   :name (if (eql (name process) local-name)
                             global-name
                             (name process))
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))))
  (:method (local-name global-name (process application-abstraction))
    (make-instance (class-of process)
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))
                   :concretion (apply-restriction local-name
                                                  global-name
                                                  (concretion process))))
  (:method (local-name global-name (process concretion))
    (make-instance (class-of process)
                   :restricted-names (restricted-names process)
                   :messages (apply-restriction local-name
                                                global-name
                                                (messages process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process)))))

(defgeneric sub-reduce (process)
  (:documentation "The sub-reduction relation is defined to handle scope
                   extrusion of restriction out of kell boundaries.")
  (:method (process)
    process)
  (:method ((process kell))
    (kell (name process) (sub-reduce (state process)) (continuation process)))
  (:method ((process restriction-abstraction))
    (let ((abstraction (abstraction process)))
      (mapc (lambda (name)
              (setf abstraction
                    ;; TODO: apply-restriction should handle all names
                    ;;       at once, rather than one at a time
                    (apply-restriction name
                                       (gensym (format nil "~a-" name))
                                       abstraction)))
            (names process))
      abstraction))
  (:method ((restriction concretion))
    (if (length (restricted-names restriction))
        (let ((messages (messages restriction))
              (continuation (continuation restriction)))
          (mapc (lambda (name)
                  (psetf messages
                         (apply-restriction name
                                            (gensym (format nil "~a-" name))
                                            messages)
                         continuation
                         (apply-restriction name
                                            (gensym (format nil "~a-" name))
                                            continuation)))
                (restricted-names restriction))
          (make-instance (class-of restriction)
                         :messages messages :continuation continuation))
        restriction))
  (:method ((process parallel-composition))
    (map-process #'sub-reduce process)))

#|
(defun delta (local-messages)
  (values (mapcar (lambda (message) (message (name message) (argument message)))
                  local-messages)
          (reduce #'compose (mapcar #'continuation local-messages))))

(defun upsilon (kells)
  (values (mapcar (lambda (kell) (kell (name kell) (state kell))) kells)
          (reduce #'compose (mapcar #'continuation kells))))

(defun psi (sub-kells)
  (values (mapcan (lambda (kell) (delta (messages kell))) sub-kells)
   (reduce #'compose (mapcar #'))))
|#

;; FIXME: move KILNS:EXECUTE-MATCH here and rename it to REDUCE
