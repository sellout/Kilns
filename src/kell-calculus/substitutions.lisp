#+xcvb (module (:depends-on ("package" "processes" "identifiers")))
(in-package :kell-calculus)

;;; FIXME: merge these and rename to SUBSTITUTE

(defgeneric replace-name (name mapping &optional ignored-vars)
  (:method (name mapping &optional ignored-vars)
    (declare (ignore mapping ignored-vars))
    name)
  (:method ((name symbol) mapping &optional ignored-vars)
    (if (find name ignored-vars)
      name
      (or (find-symbol-value name mapping)
          name)))
  (:method ((name process-variable) mapping &optional ignored-vars)
    ;; FIXME: this method only exists because sometimes '?x' is being read as a
    ;;        process-variable instead of read as a name-variable
    (if (find (name name) ignored-vars)
      name
      (find-process-variable-value name mapping))))

(defgeneric replace-variables (process mapping &optional ignored-vars)
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
    (mapcar (lambda (item) (replace-variables item mapping ignored-vars))
            process))
  (:method ((process process-variable) mapping &optional ignored-vars)
    (if (find (name process) ignored-vars)
      process
      (find-process-variable-value process mapping)))
  (:method ((process message-structure) mapping &optional ignored-vars)
    (make-instance (class-of process)
      :name (replace-name (name process) mapping ignored-vars)
      :process (replace-variables (process process) mapping ignored-vars)
      :continuation (replace-variables (continuation process) mapping
                                       ignored-vars)))
  (:method ((process parallel-composition) mapping &optional ignored-vars)
    (map-process (lambda (proc) (replace-variables proc mapping ignored-vars))
                 process))
  (:method ((process pattern-abstraction) mapping &optional ignored-vars)
    (let ((ignored-vars (append (mapcar #'name (bound-names (pattern process)))
                                (mapcar #'name
                                        (bound-variables (pattern process)))
                                ignored-vars)))
      (make-instance (class-of process)
        :pattern (make-instance 'pattern
                   :local-message-pattern
                   (replace-variables (local-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :down-message-pattern
                   (replace-variables (down-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :up-message-pattern
                   (replace-variables (up-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :kell-message-pattern
                   (replace-variables (kell-message-pattern (pattern process))
                                      mapping ignored-vars))
        :process (replace-variables (process process) mapping ignored-vars))))
  (:method ((process restriction-abstraction) mapping &optional ignored-vars)
    ;;; FIXME: these names should only be added to ignored NAME vars, but we
    ;;;        currently don't distinguish
    (let ((ignored-vars (append (names process) ignored-vars)))
      (make-instance (class-of process)
        :names (names process)
        :abstraction (replace-variables (abstraction process) mapping
                                        ignored-vars)))))