(in-package #:kilns)

(defclass agent ()
  ())

(defclass process (agent)
  ;; FIXME: really only a property of _active_ processes …
  ((parent :accessor parent)
   (deadp :initform nil :accessor deadp
          :documentation "After a message has been successfully matched, it may
                          still exist in the event queue. This ensures we don't
                          waste time trying to match it again.")))

(deftype generic-process ()
  "This allows us to use various primitives as processes."
  `(or process string number list))

(defclass concretion (agent)
  ((restricted-names)
   (messages :type multiset :documentation "A multiset without up-mossages")
   (continuation :type generalized-process)))

(defclass abstraction (agent)
  ())

(defclass simple-abstraction (abstraction)
  ())

(defclass kell-abstraction (abstraction)
  ((name)
   (process :type simple-abstraction)
   (continuation :type generalized-process)))

(defclass application-abstraction (abstraction)
  ((abstraction :type abstraction)
   (concretion :type concretion)))

(defclass restriction-abstraction (abstraction)
  ((names)
   (abstraction :type abstraction)))

(defclass pattern-abstraction (simple-abstraction)
  ((pattern :type pattern)
   (process :type generalized-process)))

(defclass simple-application-abstraction (simple-abstraction)
  ((abstraction :type simple-abstraction)
   (concretion :type concretion)))

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
      (trigger-process agent1)
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
      (trigger-process agent1)
      (activate-continuation (continuation agent2))))
  ;; The remaining methods eliminate any restrictions that might be hiding valid
  ;; applications.
  (:method ((agent1 restriction-abstraction) (agent2 restriction-abstraction))
    (@ (apply-restriction agent1) (apply-restriction agent2)))
  (:method ((agent1 abstraction) (agent2 restriction-abstraction))
    (@ agent1 (apply-restriction agent2)))
  (:method ((agent1 restriction-abstraction) (agent2 concretion))
    (@ (apply-restriction agent1) agent2)))


;;; TODO: With abstraction, we should now have a better DEF that doesn't rely on
;;;       the CL macro facility - but the names are still global ...
#|
(defmacro trigger (pattern process)
  `(make-instance 'pattern-abstraction :pattern pattern :process process))

(defmacro new (names process)
  `(make-instance 'restriction-abstraction :names names :abstraction process))

(defmacro named-abstraction (name abstraction)
  `(defmacro ,name (concretion)
     `(@ ,,abstraction ,concretion)))

(defmacro def (name parameters process)
  `(defmacro ,name (,@parameters)
     `(@ (make-instance 'pattern-abstraction
                        :pattern (list ,,@parameters) :process ,,process)
         (make-instance 'concretion :messages ,,@parameters))))
|#
