#+xcvb (module (:depends-on ("package" "processes" "identifiers")))
(in-package :kell-calculus)

(defgeneric substitute (process mapping &optional ignored-vars)
  (:method :around ((process process) mapping &optional ignored-vars)
    "Make sure conses are evaled as soon as there are no free vars, but no
     sooner."
    (declare (ignore mapping ignored-vars))
    (let ((new-process (call-next-method)))
      (map-process (lambda (proc)
                     (if (and (listp proc) (not (free-variables proc)))
                       (eval proc)
                       proc))
                   new-process)))
  (:method (process mapping &optional ignored-vars)
    "This just skips over primitives."
    (declare (ignore mapping ignored-vars))
    process)
  (:method ((name symbol) mapping &optional ignored-vars)
    (if (find name ignored-vars)
      name
      (or (find-symbol-value name mapping)
          name)))
  (:method ((process cons) mapping &optional ignored-vars)
    (mapcar (lambda (item) (substitute item mapping ignored-vars)) process))
  (:method ((process process-variable) mapping &optional ignored-vars)
    (if (find (name process) ignored-vars)
      process
      (find-process-variable-value process mapping)))
  (:method ((process message) mapping &optional ignored-vars)
    (make-instance 'message
      :name (substitute (name process) mapping ignored-vars)
      :argument (substitute (argument process) mapping ignored-vars)
      :continuation (substitute (continuation process) mapping ignored-vars)))
  (:method ((process kell) mapping &optional ignored-vars)
    (make-instance 'kell
      :name (substitute (name process) mapping ignored-vars)
      :state (substitute (state process) mapping ignored-vars)
      :continuation (substitute (continuation process) mapping ignored-vars)))
  (:method ((process parallel-composition) mapping &optional ignored-vars)
    (map-process (lambda (proc) (substitute proc mapping ignored-vars))
                 process))
  (:method ((process pattern-abstraction) mapping &optional ignored-vars)
    (let ((ignored-vars (append (mapcar #'name (bound-names (pattern process)))
                                (mapcar #'name
                                        (bound-variables (pattern process)))
                                ignored-vars)))
      (make-instance (class-of process)
        :pattern (make-instance 'pattern
                   :local-message-pattern
                   (substitute (local-message-pattern (pattern process))
                               mapping ignored-vars)
                   :down-message-pattern
                   (substitute (down-message-pattern (pattern process))
                               mapping ignored-vars)
                   :up-message-pattern
                   (substitute (up-message-pattern (pattern process))
                               mapping ignored-vars)
                   :kell-message-pattern
                   (substitute (kell-message-pattern (pattern process))
                               mapping ignored-vars))
        :process (substitute (process process) mapping ignored-vars))))
  (:method ((process restriction-abstraction) mapping &optional ignored-vars)
    ;;; FIXME: these names should only be added to ignored NAME vars, but we
    ;;;        currently don't distinguish
    (let ((ignored-vars (append (names process) ignored-vars)))
      (make-instance (class-of process)
        :names (names process)
        :abstraction (substitute (abstraction process) mapping ignored-vars)))))
