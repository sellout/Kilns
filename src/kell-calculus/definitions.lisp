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

(defmethod print-object ((obj definition) stream)
  (format stream "(define (~A ~S) ~S)"
          (name obj)
          (pattern obj)
          (process obj)))

(defclass named-concretion (concretion)
  ((name :initform (error "need a name!") :initarg :name :reader name)
   ;;      implementation details – these fields hold on to information we need
   ;;      when the concretion is expanded, but that is made available at other
   ;;      times (read time, trigger substitution time, and when other
   ;;      concretions are expanded.
   ;;  NB  Not sure if these should be here or in CONCRETION. Time will tell.
   (lexical-names :initform nil :initarg :lexical-names :reader lexical-names)
   (lexical-placeholders :initform nil :initarg :lexical-placeholders
                         :reader lexical-placeholders)
   (suspended-values :initform nil :initarg :suspended-values
                     :reader suspended-values)))

(defmethod print-object ((obj named-concretion) stream)
  (format stream "(~A~{ ~S~})"
          (name obj)
          (mapcar #'argument
                  (sort (copy-list (messages-in (messages obj))) #'<
                        :key (alexandria:compose #'label #'name)))))

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
            (compose (resolve-placeholders (substitute (process abstraction)
                                                       substitutions)
                                           (lexical-names concretion)
                                           (lexical-placeholders concretion)
                                           (suspended-values concretion))
                     (continuation concretion))
            (error "could not reduce ~A and ~A" abstraction concretion)
            ;concretion
            ))
      (compose abstraction concretion)))

(defvar *force-resolution-p* nil
  "This indicates whether we should recursively resolve all placeholders through
   concretions.")

;; This is for the rare definition that is a function and not a definition.
;; Think of these as additional special forms.
(defmethod @ ((abstraction function) (concretion named-concretion))
  (compose (funcall abstraction concretion) (continuation concretion)))

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

(defun maybe-find-in-suspended-values (ident suspended-values)
  (loop for mapping in suspended-values
     for value = (kilns::find-identifier-value ident mapping)
     until value
     finally (return (if value
                         ;; FIXME: Not sure if this is right.
                         (resolve-placeholders value lexical-names lexical-processes suspended-values)
                         ident))))

(defgeneric resolve-placeholders
    (process lexical-names lexical-processes suspended-values)
  (:documentation "Since concretions can't know what the _real_ lexical
                   environments of their messages are until after expansion, we
                   specially handle resolving them here.")
  (:method (process lexical-names lexical-processes suspended-values)
    (declare (ignore lexical-names lexical-processes suspended-values))
    (values process nil))
  (:method ((process symbol) lexical-names lexical-processes suspended-values)
    (values (let ((ident (if kilns::*reading-name-p*
                             (or (kilns::find-name process lexical-names)
                                 (kilns::find-or-add-global-name process))
                             (kilns::find-process-variable process lexical-processes))))
              (if (typep ident 'variable)
                  (loop for mapping in suspended-values
                     for value = (kilns::find-identifier-value ident mapping)
                     until value
                     finally (return (if value
                                         ;; FIXME: Not sure if this is right.
                                         (resolve-placeholders value lexical-names lexical-processes suspended-values)
                                         ident)))
                  ident))
            t))
  (:method ((process named-concretion)
            lexical-names lexical-processes suspended-values)
    (values (make-instance 'named-concretion
                           :name (if *force-resolution-p*
                                     (resolve-placeholders (name process)
                                                           lexical-names
                                                           lexical-processes
                                                           suspended-values)
                                     (name process))
                           :messages (if *force-resolution-p*
                                         (resolve-placeholders (messages process)
                                                               lexical-names
                                                               lexical-processes
                                                               suspended-values)
                                         (messages process))
                           :continuation (resolve-placeholders (continuation process)
                                                               lexical-names
                                                               lexical-processes
                                                               suspended-values)
                           :lexical-names (append lexical-names (lexical-names process))
                           :lexical-placeholders (append lexical-processes (lexical-placeholders process))
                           :suspended-values (append suspended-values (suspended-values process)))
            t))
  (:method (process lexical-names lexical-processes suspended-values)
    (declare (ignore lexical-names lexical-processes suspended-values))
    (values process nil))
  (:method ((process message) lexical-names lexical-processes suspended-values)
    (multiple-value-bind (new-name res-name-p)
        (let ((kilns::*reading-name-p* t))
          (declare (special kilns::*reading-name-p*))
          (resolve-placeholders (name process)
                                lexical-names
                                lexical-processes
                                suspended-values))
      (multiple-value-bind (new-arg res-arg-p)
          (resolve-placeholders (argument process)
                                lexical-names
                                lexical-processes
                                suspended-values)
        (multiple-value-bind (new-cont res-cont-p)
            (if (member (continuation process) '(up down))
                ;;  NB  Don't try to expand our up/down pattern markers
                (values (continuation process) nil)
                (resolve-placeholders (continuation process)
                                      lexical-names
                                      lexical-processes
                                      suspended-values))
          (if (or res-name-p res-arg-p res-cont-p)
              (values (make-instance 'message
                                     :name new-name
                                     :argument new-arg
                                     :continuation new-cont)
                      t)
              (values process nil))))))
  (:method ((process kell) lexical-names lexical-processes suspended-values)
    (multiple-value-bind (new-name res-name-p)
        (let ((kilns::*reading-name-p* t))
          (declare (special kilns::*reading-name-p*))
          (resolve-placeholders (name process)
                                lexical-names
                                lexical-processes
                                suspended-values))
      (multiple-value-bind (new-state res-state-p)
          (resolve-placeholders (state process)
                                lexical-names
                                lexical-processes
                                suspended-values)
        (multiple-value-bind (new-cont res-cont-p)
            (resolve-placeholders (continuation process)
                                  lexical-names
                                  lexical-processes
                                  suspended-values)
          (if (or res-name-p res-state-p res-cont-p)
              (values (make-instance 'kell
                                     :name new-name
                                     :state new-state
                                     :continuation new-cont)
                      t)
              (values process nil))))))
  (:method ((process parallel-composition)
            lexical-names lexical-processes suspended-values)
    (let* ((resolvedp nil)
           (new-processes
            (map-parallel-composition (lambda (proc)
                                        (multiple-value-bind (new-proc resp)
                                            (resolve-placeholders proc
                                                                  lexical-names
                                                                  lexical-processes
                                                                  suspended-values)
                                          (when resp
                                            (setf resolvedp t))
                                          new-proc))
                                      process)))
      (values (if resolvedp
                  (apply #'parallel-composition new-processes)
                  process)
              resolvedp)))
  (:method ((process restriction-abstraction)
            lexical-names lexical-processes suspended-values)
    (multiple-value-bind (new-abs resolvedp)
        (resolve-placeholders (abstraction process)
                              (cons (names process) lexical-names)
                              lexical-processes
                              suspended-values)
      (values (if resolvedp
                  (make-instance (class-of process)
                                 :names (names process)
                                 :abstraction new-abs)
                  process)
              resolvedp)))
  (:method ((process pattern-abstraction)
            lexical-names lexical-processes suspended-values)
    (multiple-value-bind (new-patt res-patt-p)
        (resolve-placeholders (pattern process)
                              lexical-names
                              lexical-processes
                              suspended-values)
      (multiple-value-bind (new-proc res-proc-p)
          (resolve-placeholders (process process)
                                (cons (bound-names new-patt) lexical-names)
                                (cons (bound-variables new-patt)
                                      lexical-processes)
                                suspended-values)
        (if (or res-patt-p res-proc-p)
            (values (make-instance (class-of process)
                                   :pattern new-patt :process new-proc)
                    t)
            (values process nil))))))
