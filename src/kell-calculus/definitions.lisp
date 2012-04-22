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
  ((name :initarg :name :reader name)))

(defmethod print-object ((obj named-concretion) stream)
  (format stream "(~A ~S)" (name obj) (messages obj)))

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

(defmethod compose ((agent1 named-concretion) (agent2 process))
  (compose (compose (make-instance 'parallel-composition) agent1)
           agent2))
(defmethod compose ((agent1 named-concretion) (agent2 null-process))
  agent1)
(defmethod compose ((agent1 parallel-composition) (agent2 named-concretion))
  (let ((pc (make-instance 'parallel-composition)))
    (psetf (process-variables pc) (process-variables agent1)
           (messages pc) (messages agent1)
           (kells pc) (kells agent1)
           (triggers pc) (triggers agent1)
           (named-concretions pc) (cons agent2 (named-concretions agent1))
           (primitives pc) (primitives agent1))
    pc))

(defmethod @ ((abstraction definition) (concretion named-concretion))
  (if (eq (name abstraction) (name concretion))
      (let ((substitutions (match (pattern abstraction) (messages concretion))))
        (if substitutions
            (compose (substitute (process abstraction) substitutions)
                     (continuation concretion))
            (compose abstraction concretion)))
      (compose abstraction concretion)))

(defmethod @ ((abstraction definition) (concretion concretion))
  (compose abstraction concretion))

(defmethod @ ((abstraction pattern-abstraction) (concretion named-concretion))
  (compose abstraction concretion))
