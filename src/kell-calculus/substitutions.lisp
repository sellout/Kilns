#+xcvb (module (:depends-on ("package" "processes" "identifiers")))
(in-package :kell-calculus)

(defgeneric substitute (process mapping &optional ignored-vars)
  (:documentation "Returns two values: the new process, and whether any
                   substitutions were made.")
  (:method-combination contract)
  (:method :guarantee (process mapping &optional ignored-vars)
    (declare (ignore mapping ignored-vars))
    (and (= (length (multiple-value-list (results))) 2)
         (multiple-value-bind (new-process substitutedp) (results)
           (not (eq substitutedp (eq process new-process))))))
  (:method (process mapping &optional ignored-vars)
    "This just skips over primitives and variable bindings."
    (declare (ignore mapping ignored-vars))
    (values process nil))
  (:method ((name symbol) mapping &optional ignored-vars)
    (if (find name ignored-vars)
        (values name nil)
        (let ((new-process (find-symbol-value name mapping)))
          (if new-process
              (values new-process t)
              (values name nil)))))
  (:method ((process message) mapping &optional ignored-vars)
    (multiple-value-bind (new-name sub-name-p)
        (substitute (name process) mapping ignored-vars)
      (multiple-value-bind (new-arg sub-arg-p)
          (substitute (argument process) mapping ignored-vars)
        (multiple-value-bind (new-cont sub-cont-p)
            (substitute (continuation process) mapping ignored-vars)
          (if (or sub-name-p sub-arg-p sub-cont-p)
              (values (make-instance 'message
                                     :name new-name
                                     :argument new-arg
                                     :continuation new-cont)
                      t)
              (values process nil))))))
  (:method ((process kell) mapping &optional ignored-vars)
    (multiple-value-bind (new-name sub-name-p)
        (substitute (name process) mapping ignored-vars)
      (multiple-value-bind (new-state sub-state-p)
          (substitute (state process) mapping ignored-vars)
        (multiple-value-bind (new-cont sub-cont-p)
            (substitute (continuation process) mapping ignored-vars)
          (if (or sub-name-p sub-state-p sub-cont-p)
              (values (make-instance 'kell
                                     :name new-name
                                     :state new-state
                                     :continuation new-cont)
                      t)
              (values process nil))))))
  (:method ((process named-concretion) mapping &optional ignored-vars)
    (multiple-value-bind (new-messages substitutedp)
        (substitute (messages process) mapping ignored-vars)
      (multiple-value-bind (new-name sub-name-p)
          (if (typep (name process) 'named-concretion)
              (substitute (name process) mapping ignored-vars)
              (values (name process) nil))
        (values (if (or substitutedp sub-name-p)
                    (make-instance 'named-concretion
                                   :name new-name :messages new-messages)
                    process)
                (or substitutedp sub-name-p)))))
  (:method ((process parallel-composition) mapping &optional ignored-vars)
    (let* ((substitutedp nil)
           (new-processes (map-parallel-composition (lambda (proc)
                                                      (multiple-value-bind (new-process subp)
                                                          (substitute proc mapping
                                                                      ignored-vars)
                                                        (when subp (setf substitutedp t))
                                                        new-process))
                                                    process)))
      (values (if substitutedp
                  (apply #'parallel-composition new-processes)
                  process)
              substitutedp)))
  (:method ((process pattern-abstraction) mapping &optional ignored-vars)
    (let ((ignored-vars (append (mapcar #'name (bound-names (pattern process)))
                                (mapcar #'name
                                        (bound-variables (pattern process)))
                                ignored-vars)))
      (multiple-value-bind (new-patterns sub-patterns-p)
          (substitute (pattern process) mapping ignored-vars)
        (multiple-value-bind (new-proc sub-proc-p)
            (substitute (process process) mapping ignored-vars)
          (if (or sub-patterns-p sub-proc-p)
              (values (make-instance (class-of process)
                                     :pattern new-patterns :process new-proc)
                      t)
              (values process nil))))))
  (:method ((process pattern) mapping &optional ignored-vars)
    (let ((substitutedp nil))
      (flet ((substitute-pattern (subpattern)
               (multiple-value-bind (new-pattern subp)
                   (substitute subpattern mapping ignored-vars)
                 (when subp (setf substitutedp t))
                 new-pattern)))
        (let ((new-patterns (mapcar #'substitute-pattern
                                    (placeholders process)))
              (new-locals (mapcar #'substitute-pattern
                                    (local-message-pattern process)))
              (new-ups (mapcar #'substitute-pattern
                               (up-message-pattern process)))
              (new-downs (mapcar #'substitute-pattern
                                 (down-message-pattern process)))
              (new-kells (mapcar #'substitute-pattern
                                 (kell-message-pattern process)))
              (new-concres (mapcar #'substitute-pattern
                                   (named-concretions process))))
          (values (if substitutedp
                      (let ((new-messages (remove-if-not (alexandria:rcurry #'typep 'message)
                                                         new-patterns))
                            (new-kell-pattern (remove-if-not (alexandria:rcurry #'typep 'kell)
                                                         new-patterns))
                            (new-placeholders (remove-if-not (alexandria:rcurry #'typep
                                                                                'symbol)
                                                             new-patterns))
                            (new-concretions (remove-if-not (alexandria:rcurry #'typep
                                                                                'named-concretion)
                                                             new-patterns)))
                        (let ((new-local-pattern (remove-if-not (alexandria:rcurry #'typep 'null-process)
                                                                new-messages
                                                                :key #'continuation))
                              (new-up-pattern (remove-if-not (alexandria:curry #'eq 'up)
                                                             new-messages
                                                             :key #'continuation))
                              (new-down-pattern (remove-if-not (alexandria:curry #'eq 'down)
                                                               new-messages
                                                               :key #'continuation)))
                          (make-instance 'pattern
                                         :local-message-pattern (append new-locals
                                                                        new-local-pattern)
                                         :down-message-pattern (append new-downs new-down-pattern)
                                         :up-message-pattern (append new-ups new-up-pattern)
                                         :kell-message-pattern (append new-kells new-kell-pattern)
                                         :named-concretions (append new-concres new-concretions)
                                         :placeholders new-placeholders)))
                      process)
                  substitutedp)))))
  (:method ((process restriction-abstraction) mapping &optional ignored-vars)
    ;;; FIXME: these names should only be added to ignored NAME vars, but we
    ;;;        currently don't distinguish
    (let ((ignored-vars (append (names process) ignored-vars)))
      (multiple-value-bind (new-abstraction substitutedp)
          (substitute (abstraction process) mapping ignored-vars)
        (values (if substitutedp
                    (make-instance (class-of process)
                                   :names (names process)
                                   :abstraction new-abstraction)
                    process)
                substitutedp)))))
