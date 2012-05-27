(in-package #:kell-calculus)

(defclass agent ()
  ((parent :accessor parent)
   (deadp :initform nil :accessor deadp
          :documentation "After a message has been successfully matched, it may
                          still exist in the event queue. This ensures we don't
                          waste time trying to match it again.")))

(deftype generic-process ()
  "This allows us to use various primitives as processes."
  `(or agent string number symbol))

(deftype generic-abstraction ()
  "This allows us to use various primitives as processes."
  `(or abstraction generic-process))

(deftype generic-concretion ()
  "This allows us to use various primitives as processes."
  `(or concretion generic-process))

(deftype generic-agent ()
  "This allows us to use various primitives as processes."
  `(or generic-abstraction generic-concretion))

(defclass concretion (agent)
  ((restricted-names :initform nil
                     :initarg :restricted-names :reader restricted-names)
   (messages :initform +null-process+ :initarg :messages :reader messages
             :type generic-process
             :documentation "A multiset without up-messages")
   (continuation :initform +null-process+
                 :initarg :continuation :reader continuation
                 :type process))
  (:documentation "C ::= νã.Ω P
                   Ω ::= ∅ | a<P> | a<P>↓b | a[P] | Ω|Ω"))

(defmethod print-object ((obj concretion) stream)
  (format stream "(new ~s ~s || ~s)"
          (restricted-names obj)
          (messages obj)
          (continuation obj)))

#|
(defmethod free-names ((agent down-message))
  (union (free-names (name agent))
         (free-names (argument agent))
         (free-names (parent agent))))

(defmethod free-variables ((agent down-message))
  (free-variables (argument agent)))
|#

(defmethod free-names ((agent concretion))
  (union (free-names (messages agent))
         (free-names (continuation agent))))

(defmethod free-variables ((agent concretion))
  (union (free-variables (messages agent))
         (free-variables (continuation agent))))

(defclass abstraction (agent)
  ()
  (:documentation "F"))

(defclass simple-abstraction (abstraction)
  ()
  (:documentation "G"))

(defclass kell-abstraction (abstraction)
  ((name)
   (abstraction :reader abstraction :type simple-abstraction)
   (continuation :reader continuation :type process))
  (:documentation "a[G].P"))

(defmethod print-object ((obj kell-abstraction) stream)
  (format stream "[~s ~s ~s]"
          (name obj)
          (abstraction obj)
          (continuation obj)))

(defclass application-abstraction (abstraction)
  ((abstraction :initarg :abstraction :reader abstraction
                :type generic-abstraction)
   (concretion :initarg :concretion :reader concretion
               :type generic-concretion))
  (:documentation "F@C"))

(defmethod print-object ((obj application-abstraction) stream)
  (format stream "(@ ~s ~s)" (abstraction obj) (concretion obj)))

(defmethod free-names ((agent application-abstraction))
  ;; FIXME: only when defined (see 3.3.Pseudo-application)
  (union (free-names (abstraction agent)) (free-names (concretion agent))))

(defmethod free-variables ((agent application-abstraction))
  ;; FIXME: only when defined (see 3.3.Pseudo-application)
  (union (free-variables (abstraction agent))
         (free-variables (concretion agent))))

(defclass restriction-abstraction (abstraction)
  ((names :initarg :names :reader names)
   (abstraction :initarg :abstraction :reader abstraction :type abstraction))
  (:documentation "νã.F"))

(defmethod print-object ((obj restriction-abstraction) stream)
  (format stream "(new ~s ~s)" (names obj) (abstraction obj)))

(defclass pattern-abstraction (simple-abstraction)
  ((pattern :reader pattern :type pattern)
   (process :initarg :process :reader process :type generic-process))
  (:documentation "(ξ)P"))

(defmethod initialize-instance :after
    ((instance pattern-abstraction) &key pattern &allow-other-keys)
  (let ((pat (convert-process-to-pattern pattern)))
    ;(break "converted ~A into ~A" pattern pat)
    (setf (slot-value instance 'pattern) pat)))

(defmethod print-object ((obj pattern-abstraction) stream)
  (format stream "((~s) ~s)" (pattern obj) (process obj)))

(defclass simple-application-abstraction
    (simple-abstraction application-abstraction)
  ;; FIXME: not sure if this is the right way to restrict a type
  ((abstraction :reader abstraction :type simple-abstraction))
  (:documentation "G@C"))

(defclass process (abstraction) ;; simple-abstraction?
  ()
  (:documentation "P"))

(defgeneric compose (agent1 agent2)
  (:method ((agent1 process) (agent2 abstraction)) (compose agent2 agent1))
  (:method ((agent1 abstraction) (agent2 process))
    (make-instance 'application-abstraction
      :abstraction agent1
      :concretion (make-instance 'concretion :continuation agent2)))
  (:method ((agent1 process) (agent2 concretion)) (compose agent2 agent1))
  (:method ((agent1 concretion) (agent2 process))
    (if (intersection (restricted-names agent1) (free-names agent2))
      ;;; FIXME: What to do when the intersection isn't null?
      (values)
      (make-instance 'concretion
        :restricted-names (restricted-names agent1)
        :messages (messages agent1)
        :continuation (compose (continuation agent1) agent2))))
  (:method ((agent1 concretion) (agent2 concretion))
    (if (or (intersection (restricted-names agent1) (free-names agent2))
            (intersection (restricted-names agent2) (free-names agent1)))
      ;;; FIXME: What to do when the intersection isn't null?
      (values)
      (make-instance 'concretion
        :restricted-names (append (restricted-names agent1) (restricted-names agent2))
        :messages (compose (messages agent1) (messages agent2))
        :continuation (compose (continuation agent1) (continuation agent2))))))

(defgeneric @ (agent1 agent2)
  ;; In the base cases, there is no reduction that can be done, so we suspend
  ;; the application in an APPLICATION-ABSTRACTION for the time being
  (:method ((agent1 simple-abstraction) (agent2 concretion))
    (make-instance 'simple-application-abstraction
                   :abstraction agent1 :concretion agent2))
  (:method ((agent1 abstraction) (agent2 concretion))
    (make-instance 'application-abstraction
                   :abstraction agent1 :concretion agent2))
  ;; Here is where we do the actual reductions
  (:method ((agent1 application-abstraction) (agent2 concretion))
    (@ (abstraction agent1) (compose (concretion agent1) agent2)))
  (:method ((agent1 pattern-abstraction) (agent2 concretion))
    (let ((substitutions (match (pattern agent1) (messages agent2))))
      (if substitutions
          (compose (substitute (process agent1) substitutions)
                   (continuation agent2))
          (call-next-method))))
  (:method ((agent1 kell-abstraction) (agent2 concretion))
    (let* ((nested-abstraction (abstraction (process agent1)))
           (nested-concretion (concretion (process agent1)))
           (substitutions (match (pattern nested-abstraction)
                                 (compose (messages nested-concretion)
                                          (messages agent2)))))
      (if substitutions
          (compose (make-instance
                    'kell
                    :name (name agent1)
                    :state (compose (substitute (process nested-abstraction)
                                                substitutions)
                                    (continuation nested-concretion))
                    :continuation (continuation agent1))
                   (continuation agent2))
          (call-next-method))))
  ;; The remaining methods eliminate any restrictions that might be hiding valid
  ;; applications.
  (:method ((agent1 restriction-abstraction) (agent2 restriction-abstraction))
    (@ (expand-restriction agent1) (expand-restriction agent2)))
  (:method ((agent1 abstraction) (agent2 restriction-abstraction))
    (@ agent1 (expand-restriction agent2)))
  (:method ((agent1 restriction-abstraction) (agent2 concretion))
    (@ (expand-restriction agent1) agent2)))
