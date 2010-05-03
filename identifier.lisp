#+xcvb (module (:depends-on ("package" "process")))
(in-package :kilns)

(defclass identifier ()
  )

(defclass name (identifier)
  )

(defclass process-variable (identifier process)
  )

(defgeneric substitute (source)
  (:method ((source name))
    )
  (:method ((source process-variable))
    ))