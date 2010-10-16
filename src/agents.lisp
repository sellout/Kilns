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
   (continuation :reader continuation :type generic-process))
  (:documentation "C ::= νã.Ω P
                   Ω ::= ∅ | a<P> | a<P>↓b | a[P] | Ω|Ω"))

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
  ()
  (:documentation "F"))

(defclass simple-abstraction (abstraction)
  ()
  (:documentation "G"))

(defclass kell-abstraction (abstraction)
  ((name)
   (abstraction :reader abstraction :type simple-abstraction)
   (continuation :reader continuation :type generic-process))
  (:documentation "a[G].P"))

(defclass application-abstraction (abstraction)
  ((abstraction :reader abstraction :type generic-abstraction)
   (concretion :reader concretion :type generic-concretion))
  (:documentation "F@C"))

(defmethod free-names ((agent application-abstraction))
  ;; FIXME: only when defined (see 3.3.Pseudo-application)
  (union (free-names (abstraction agent)) (free-names (concretion agent))))

(defmethod free-variables ((agent application-abstraction))
  ;; FIXME: only when defined (see 3.3.Pseudo-application)
  (union (free-variables (abstraction agent))
         (free-variables (concretion agent))))

(defclass restriction-abstraction (abstraction)
  ((names :reader names)
   (abstraction :reader abstraction :type abstraction))
  (:documentation "νã.F"))

(defclass pattern-abstraction (simple-abstraction)
  ((pattern :initarg :pattern :type pattern)
   (process :initarg :process :type generic-process))
  (:documentation "(ξ)P"))

(defclass simple-application-abstraction
    (simple-abstraction application-abstraction)
  ;; FIXME: not sure if this is the right way to restrict a type
  ((abstraction :reader abstraction :type simple-abstraction))
  (:documentation "G@C"))

(defclass process (abstraction)
  ;; FIXME: really only a property of _active_ processes …
  ((deadp :initform nil :accessor deadp
          :documentation "After a message has been successfully matched, it may
                          still exist in the event queue. This ensures we don't
                          waste time trying to match it again."))
  (:documentation "P"))

(defgeneric compose (agent1 agent2)
  (:method (agent1 agent2)
    "COMPOSE is commutative, but we somehow need to make sure we don't just keep
     switching the argument order indefinitely."
    (compose agent2 agent1))
  (:method ((agent1 abstraction) (agent2 process))
    (make-instance 'application-abstraction
                   :abstraction agent1
                   :concretion (make-instance 'concretion
                                              :continuation agent2)))
  (:method ((agent1 concretion) (agent2 process))
    (make-instance 'concretion
                   :restricted-names (restricted-names agent1)
                   :messages (messages agent1)
                   :continuation (compose (continuation agent1) agent2)))
  (:method ((agent1 concretion) (agent2 concretion))
    (make-instance 'concretion
                   :restricted-names (append (restricted-names agent1)
                                             (restricted-names agent2))
                   :messages (compose (messages agent1) (messages agent2))
                   :continuation (compose (continuation agent1)
                                          (continuation agent2)))))

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


;;; TODO: With abstraction, we should now have a better DEF that doesn't rely on
;;;       the CL macro facility - but the names are still global ...
#|
(defmacro trigger (pattern process)
  `(make-instance 'pattern-abstraction :pattern pattern :process process))

(defmacro new (names process)
  `(make-instance 'restriction-abstraction :names names :abstraction process))

(defmacro def ((name &rest parameters) process)
  (let ((concretion (gensym "CONCRETION")))
    `(defmacro ,name (&rest ,concretion)
       `(@ (make-instance 'pattern-abstraction
                          :pattern ,'(list ,@parameters) :process ,',process)
           (list ,@,concretion)))))
|#
