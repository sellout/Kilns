(in-package #:kell-calculus)

(defclass definition (pattern-abstraction)
  ((name :initarg :name :reader name)))

(defun order-processes (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  (let ((index 0))
    (reduce #'compose
            (mapcar (lambda (process) (make-instance 'message
                                                     :name (incf index)
                                                     :argument process))
                    processes))))

(defmacro def ((name &rest parameters) &body body)
  "Allows us to define new operations. It's currently just CL's DEFMACRO."
  `(progn (defmacro ,name (,@parameters)
            ,@body)
          null))

(defclass named-concretion (concretion)
  ((name :initform (error "need a name!") :initarg :name :reader name)))

(defmethod print-object ((obj named-concretion) stream)
  (format stream "(~A~{ ~S~})"
          (name obj)
          (mapcar #'argument
                  (sort (copy-list (messages-in (messages obj))) #'<
                        :key #'name))))

(defmethod convert-process-to-pattern
    ((process named-concretion) &optional (pattern (make-instance 'pattern)))
  (push process (named-concretions pattern))
  pattern)

(defgeneric named-concretions-in (process)
  (:documentation "Retrieves a list of the triggers contained in the process.")
  (:method (process)
    (declare (ignore process))
    nil)
  (:method ((process named-concretion))
    (list process))
  (:method ((process parallel-composition))
    (named-concretions process)))

(defmethod remove-process-from ((process named-concretion) kell)
  (if (find process (named-concretions-in (state kell)))
      (typecase (state kell)
        (parallel-composition (setf (named-concretions (state kell))
                                    (delete process
                                            (named-concretions (state kell))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (state kell)))
                                (0 (setf (state kell) null))
                                (1 (setf (state kell)
                                         (car (map-parallel-composition
                                               #'identity
                                               (state kell)))))))
        (t (setf (state kell) null)))
      (error "The (restriction) process ~a is not contained in ~a, and thus ~
              can not be removed."
             process (state kell))))

(defmethod compose (agent1 (agent2 definition))
  (compose agent2 agent1))
(defmethod compose ((agent1 definition) (agent2 definition))
  (compose (compose (make-instance 'parallel-composition) agent1)
           agent2))
(defmethod compose ((agent1 definition) (agent2 process))
  (compose (compose (make-instance 'parallel-composition) agent1)
           agent2))
(defmethod compose ((agent1 definition) (agent2 null-process))
  agent1)
(defmethod compose ((agent1 parallel-composition) (agent2 definition))
  (make-instance 'parallel-composition
                 :process-variables (process-variables agent1)
                 :messages (messages agent1)
                 :kells (kells agent1)
                 :triggers (triggers agent1)
                 :named-concretions (named-concretions agent1)
                 :primitives (cons agent2 (primitives agent1))))

(defmethod compose ((agent1 named-concretion) (agent2 process))
  (compose (compose (make-instance 'parallel-composition) agent1)
           agent2))
(defmethod compose ((agent1 named-concretion) (agent2 named-concretion))
  (compose (compose (make-instance 'parallel-composition) agent1)
           agent2))
(defmethod compose ((agent1 named-concretion) (agent2 null-process))
  agent1)
(defmethod compose ((agent1 parallel-composition) (agent2 named-concretion))
  (make-instance 'parallel-composition
                 :process-variables (process-variables agent1)
                 :messages (messages agent1)
                 :kells (kells agent1)
                 :triggers (triggers agent1)
                 :named-concretions (cons agent2 (named-concretions agent1))
                 :primitives (primitives agent1)))

(defmethod @ ((abstraction definition) (concretion named-concretion))
  (if (eq (name abstraction) (name concretion))
      (let ((substitutions (match (pattern abstraction) (messages concretion))))
        (if substitutions
            (compose (substitute (process abstraction) substitutions)
                     (continuation concretion))
            (error "could not reduce ~A and ~A" abstraction concretion)
            ;concretion
            ))
      (compose abstraction concretion)))

;; This is for the rare definition that is a function and not a definition.
;; Think of these as additional special forms.
(defmethod @ ((abstraction function) (concretion named-concretion))
  (compose (funcall abstraction (messages concretion))
           (continuation concretion)))

(defmethod @ ((abstraction definition) (concretion concretion))
  (compose abstraction concretion))

(defmethod @ ((abstraction pattern-abstraction) (concretion named-concretion))
  (compose abstraction concretion))

(defmethod apply-restriction (local-name global-name (process named-concretion))
  (if (member local-name (restricted-names process))
      process
      (make-instance 'named-concretion
                     :name (name process)
                     :restricted-names (restricted-names process)
                     :messages (apply-restriction local-name
                                                  global-name
                                                  (messages process))
                     :continuation (apply-restriction local-name
                                                      global-name
                                                      (continuation process)))))
