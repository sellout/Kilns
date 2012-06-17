#+xcvb (module (:depends-on ("packages" "processes")))
(in-package :kell-calculus)

;;;                     identifier
;;;                     ^        ^
;;;             name-type        variable       process
;;;             ^       ^        ^      ^             ^
;;;           name     name-variable   process-variable
;;;           ^  ^
;;; global-name  restricted-name

(defclass identifier ()
  ((label :initform (error "missing identifier label") :initarg :label
          :reader label :type (or symbol integer))))

(defclass variable (identifier)
  ())

(defmethod print-object ((obj variable) stream)
  (format stream (if kilns::*debugp* "⟨~s⟩" "~s") (label obj)))

(defclass name-type ()
  ()
  (:documentation "This gives us a class to hang pattern-language extensions to
                   names off of."))

(deftype generic-name ()
  "This basically allows for the suspended names in concretions, which don't
   have a class yet."
  `(or name-type symbol integer))

(defclass process-variable (process variable)
  ()
  (:documentation
   "These only exist in non-evaluation contexts. When a trigger is triggered, we
    convert each process-variable into its “realized” process and do so
    recursively through nested processes, except where the variable is shadowed
    by a more local variable with the same name."))

(defclass name (name-type identifier)
  ())

(defmethod print-object ((obj name) stream)
  (format stream "~s" (label obj)))

(defgeneric extrude-scope (name)
  (:documentation "Converts a name in a restriction into a globally-unique name."))

;;;  NB  Can't just change class to fix all references, because that will
;;;      also affect references that aren’t in the current context, and
;;;      those should be reified to different global names.

;;; FIX  Problem with this implementataion of identifiers: figuring out
;;;      lexical scoping at read time means that concretions that expand
;;;      into patterns (like `trigger*`) end up with the wrong scoping.
;;;  TD  A possible solution is to avoid resolving any placeholders in a
;;;      concretion until after expansion. The concretion then needs to
;;;      hold on to the current scope until expansion. This might be
;;;      problematic for the wire protocol (there are ways to improve it,
;;;      like pruning the scope of any names that don't exist in the
;;;      concretion), but we'll deal with that when we come to it.
