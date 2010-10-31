#+xcvb (module (:depends-on ("packages" "processes")))
(in-package :kell-calculus)

#| FIXME: we're currently using symbols for names, so we deftype instead of
          using these classes
(defclass identifier ()
  ())
(defclass name (identifier)
  ())
|#

(defclass name-type () ; identifier
  ()
  (:documentation "This gives us a class to hang pattern-language extensions to
                   names off of."))

(deftype name ()
  `(or symbol integer name-type))

(deftype identifier ()
  `(or name process-variable))

(defclass process-variable (process) ; identifier
  ((name :initarg :name :type symbol :reader name))
  (:documentation
   "These only exist in “potential” processes. When a trigger is triggered, we
    convert each process-variable into its “realized” process and do so
    recursively through nested processes, except where the variable is shadowed
    by a more local variable with the same name."))

(defmethod print-object ((obj process-variable) stream)
  (format stream "?~s" (name obj)))

(defun process-variable (name)
  (make-instance 'process-variable :name name))
