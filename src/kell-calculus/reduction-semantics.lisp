#+xcvb (module (:depends-on ("utilities" "syntax")))
(in-package :kell-calculus)

(defmethod structurally-congruent-p ((left pattern) (right pattern))
  (and (set= (free-names left) (free-names right))
       (set= (channel-names left) (channel-names right))
       (set= (union (bound-names left) (bound-variables left))
             (union (bound-names right) (bound-variables right)))))

(defgeneric apply-restriction (local-name global-name process &optional expandp)
  (:documentation "DESTRUCTIVE. Returns the process with all restrictions
                   expanded to have unique names.")
  (:method (local-name global-name process &optional (expandp t))
    (declare (ignore local-name global-name expandp))
    process)
  (:method (local-name global-name (process message) &optional (expandp t))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (psetf (argument process)
           (apply-restriction local-name global-name (argument process) expandp)
           (continuation process)
           (apply-restriction local-name global-name (continuation process)
                              expandp))
    process)
  (:method (local-name global-name (process kell) &optional (expandp t))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (psetf (state process)
           (apply-restriction local-name global-name (state process) expandp)
           (continuation process)
           (apply-restriction local-name global-name (continuation process)
                              expandp))
    process)
  (:method (local-name global-name (process parallel-composition)
            &optional (expandp t))
    (apply #'parallel-composition
           (map-parallel-composition (lambda (proc)
                                       (apply-restriction local-name
                                                          global-name
                                                          proc
                                                          expandp))
                                     process)))
  (:method (local-name global-name (process pattern) &optional (expandp t))
    (psetf (local-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (local-message-pattern process))
           (down-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (down-message-pattern process))
           (up-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (up-message-pattern process))
           (kell-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (kell-message-pattern process)))
    process)
  (:method (local-name global-name (process restriction-abstraction)
            &optional (expandp t))
    (if expandp
      (apply-restriction local-name global-name (expand-restriction process)
                         expandp)
      (if (find local-name (names process))
        process
        (make-instance (class-of process)
          :names (names process)
          :abstraction (apply-restriction local-name
                                          global-name
                                          (abstraction process)
                                          expandp)))))
  (:method (local-name global-name (process pattern-abstraction)
            &optional (expandp t))
    (make-instance (class-of process)
      :pattern (apply-restriction local-name global-name (pattern process)
                                  expandp)
      :process (apply-restriction local-name global-name (process process)
                                  nil))))

(defgeneric expand-restriction (restriction)
  (:method ((restriction restriction-abstraction))
    (let ((abstraction (abstraction restriction)))
      (mapc (lambda (name)
              (setf abstraction
                    ;; TODO: apply-restriction should handle all names
                    ;;       at once, rather than one at a time
                    (apply-restriction name
                                       (gensym (format nil "~a-" name))
                                       abstraction)))
            (names restriction))
      abstraction))
  (:method ((restriction concretion))
    (if (length (restricted-names restriction))
        (let ((messages (messages restriction))
              (continuation (continuation restriction)))
          (mapc (lambda (name)
                  (psetf messages
                         (apply-restriction name
                                            (gensym (format nil "~a-"
                                                            name))
                                            messages)
                         continuation
                         (apply-restriction name
                                            (gensym (format nil "~a-" name))
                                            continuation)))
                (restricted-names restriction))
          (make-instance (class-of restriction)
                         :messages messages :continuation continuation))
        restriction)))

(defgeneric sub-reduce (process)
  (:documentation "The sub-reduction relation is defined to handle scope
                   extrusion of restriction out of kell boundaries.")
  (:method (process)
    process)
  (:method ((process kell))
    (kell (name process) (sub-reduce (state process)) (continuation process)))
  (:method ((process restriction))
    (expand-restriction (make-instance 'restriction
                                       :names (names process)
                                       :abstraction
                                       (sub-reduce (abstraction process)))))
  (:method ((process parallel-composition))
    (map-process #'sub-reduce process)))

;; FIXME: move KILNS:EXECUTE-MATCH here and rename it to REDUCE
