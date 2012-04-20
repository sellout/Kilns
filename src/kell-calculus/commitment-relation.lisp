#+xcvb (module (:depends-on ("package" "agents" "processes")))
(in-package #:kell-calculus)

(defgeneric commit (process)
  (:method ((process message))
    (make-instance 'concretion
                   :messages (make-instance 'message
                                            :name (name process)
                                            :argument (argument process))
                   :continuation (continuation process)))
  (:method ((process kell))
    (let ((reduced-kell (sub-reduce process)))
      (make-instance 'concretion
                     :messages (make-instance 'kell
                                              :name (name reduced-kell)
                                              :state (state reduced-kell))
                     :continuation (continuation reduced-kell))))
  (:method ((process trigger))
    (make-instance 'pattern-abstraction
                   :pattern (pattern process)
                   :abstraction (process process)))
  (:method ((process restriction))
    (make-instance 'restriction-abstraction
                   :names (name process)
                   :process (process process)))
  (:method ((process kell))
    ;; FIXME: need to merge this with the previous kell definition
    (let* ((reduced-kell (sub-reduce process))
           (state (commit (state reduced-kell))))
      (etypecase state
        (process (make-instance 'kell
                                :name (name reduced-kell)
                                :state state
                                :continuation (continuation reduced-kell)))
        (concretion
         (make-instance 'concretion
                        :messages (make-instance 'message
                                                 :name (name state)
                                                 :argument (process state)
                                                 :continuation (name reduced-kell))
                        :continuation (make-instance 'kell
                                                     :name (name reduced-kell)
                                                     :state (continuation state)
                                                     :continuation (continuation reduced-kell))))
        (simple-abstraction
         (make-instance 'kell-abstraction
                        :name (name reduced-kell)
                        :process state
                        :continuation (continuation reduced-kell))))))
  (:method ((process parallel-composition))
    (reduce #'compose (map-parallel-composition #'commit process))))
