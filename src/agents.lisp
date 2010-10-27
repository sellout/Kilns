(in-package #:kilns)

(defclass agent ()
  ((parent :accessor parent)))

(deftype generic-process ()
  "This allows us to use various primitives as processes."
  `(or process string number list symbol))

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
  ((restricted-names :reader restricted-names)
   (messages :reader messages :type multiset
             :documentation "A multiset without up-mossages")
   (continuation :reader continuation :type process))
  (:documentation "C ::= νã.Ω P
                   Ω ::= ∅ | a<P> | a<P>↓b | a[P] | Ω|Ω"))

(defmethod print-object ((obj concretion) stream)
  ;; FIXME: print messages as a parallel-composition
  (format stream "(new ~s ~s ~s)"
          (restricted-names obj) (messages obj) (continuation obj)))

#|
(defmethod free-names ((agent down-message))
  (union (free-names (name agent))
         (free-names (process agent))
         (free-names (parent agent))))

(defmethod free-variables ((agent down-message))
  (free-variables (process agent)))
|#

(defmethod free-names ((agent concretion))
  (union (free-names (messages agent))
         (free-names (continuation agent))))

(defmethod free-variables ((agent concretion))
  (union (free-variables (messages agent))
         (free-variables (continuation agent))))

(defclass abstraction (agent)
  ((deadp :initform nil :accessor deadp
          :documentation "After a message has been successfully matched, it may
                          still exist in the event queue. This ensures we don't
                          waste time trying to match it again."))
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
  ((abstraction :reader abstraction :type generic-abstraction)
   (concretion :reader concretion :type generic-concretion))
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
  ((pattern :initarg :pattern :reader pattern :type pattern)
   (process :initarg :process :reader process :type generic-process))
  (:documentation "(ξ)P"))

(defmethod print-object ((obj pattern-abstraction) stream)
  ;; FIXME: need a better syntax
  (format stream "(pat ~s ~s)" (pattern obj) (process obj)))

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
    (if (or (intersection (restricted-names agent1)
                          (union (free-names (messages agent2))
                                 (free-names (continuation agent2))))
            (intersection (restricted-names agent2)
                          (union (free-names (messages agent1))
                                 (free-names (continuation agent1)))))
      ;;; FIXME: What to do when the intersection isn't null?
      (values)
      (make-instance 'concretion
        :restricted-names (append (restricted-names agent1) (restricted-names agent2))
        :messages (compose (messages agent1) (messages agent2))
        :continuation (compose (continuation agent1) (continuation agent2))))))

(defgeneric apply-abstraction (abstraction)
  (:method ((abstraction application-abstraction))
    (@ (abstraction abstraction) (concretion abstraction))))

(defgeneric @ (agent1 agent2)
  (:method ((agent1 application-abstraction) (agent2 concretion))
    (@ (abstraction agent1) (compose (concretion agent1) agent2)))
  (:method ((agent1 pattern-abstraction) (agent2 concretion))
    (destructuring-bind (processes substitutions)
        (match (pattern agent1) (messages agent2))
      (mapc #'remove-process processes)
      (trigger-process agent1 substitutions)
      (activate-continuation (continuation agent2))))
  (:method ((agent1 kell-abstraction) (agent2 concretion))
    (destructuring-bind (processes substitutions)
        (match (pattern (abstraction (process agent1)))
               (compose (messages (concretion (process agent1)))
                        (messages agent2)))
      (let ((kell (make-instance 'kell
                                 :name (name agent1)
                                 :continuation (continuation agent1))))
        (setf (parent agent1) kell))
      (mapc #'remove-process processes)
      (trigger-process agent1 substitutions)
      (activate-continuation (continuation agent2))))
  ;; The remaining methods eliminate any restrictions that might be hiding valid
  ;; applications.
  (:method ((agent1 restriction-abstraction) (agent2 restriction-abstraction))
    (@ (expand-restriction agent1) (expand-restriction agent2)))
  (:method ((agent1 abstraction) (agent2 restriction-abstraction))
    (@ agent1 (expand-restriction agent2)))
  (:method ((agent1 restriction-abstraction) (agent2 concretion))
    (@ (expand-restriction agent1) agent2)))

(defmacro def ((name &rest parameters) &body body)
  "Allows us to define new operations. It's currently just like CL's DEFMACRO, but
   hopefully I can improve on that."
  `(defmacro ,name (,@parameters)
     ,@body))

#|
(defmacro def ((name &rest parameters) process)
  `(defmacro ,name (&rest messages)
     `(@ ,(make-instance 'pattern-abstraction
            :pattern '(list ,@parameters) :process ,process)
         (make-instance 'concretion :messages (list ,@,messages)))))
|#
