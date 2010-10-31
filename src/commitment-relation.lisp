#+xcvb (module (:depends-on ("package" "agents" "processes")))
(in-package #:kell-calculus)

(defgeneric commit (process)
  (:method ((process message))
    (make-instance 'concretion
                   :messages (message (name process) (process process))
                   :continuation (continuation process)))
  (:method ((process kell))
    (let ((reduced-kell (sub-reduce process)))
      (make-instance 'concretion
                     :messages (kell (name reduced-kell) (process reduced-kell))
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
           (inner-process (commit (process reduced-kell))))
      (etypecase inner-process
        (process (kell (name reduced-kell)
                       inner-process
                       (continuation reduced-kell)))
        (concretion
         (make-instance 'concretion
                        :messages (message (name inner-process)
                                           (process inner-process)
                                           (name reduced-kell))
                        :continuation (kell (name reduced-kell)
                                            (continuation inner-process)
                                            (continuation reduced-kell))))
        (simple-abstraction
         (make-instance 'kell-abstraction
                        :name (name reduced-kell)
                        :process inner-process
                        :continuation (continuation reduced-kell))))))
  (:method ((process parallel-composition))
    (reduce #'compose (map-parallel-composition #'commit process))))
