#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; We present in this section a calculus, called FraKtal, in which we can model
;;; several interesting features of a recent reflective component model called
;;; Fractal [5]. Fractal provides traditional notions of commponent-based
;;; software-engineering, namely components with input and output interfaces (or
;;; ports), which can be explicitly connected or disconnected during execution
;;; by means of bindings (or connectors). In addition, FraKtal allows different
;;; forms of component introspection and intercession, such as adding and
;;; removing subcomponents, adding and removing interceptors on interfaces,
;;; controlling a component execution and life-cycle, etc. Interestingly, the
;;; reflective features in the Fractal model are introduced by means of a
;;; general component structure which dis- tinguishes between the component
;;; membrane, which contains all the control functions, and the component
;;; content, which consists of other components (the subcomponents). This
;;; distinction between component membrane and component content is not
;;; dissimilar to that of the generic membrane model mentioned above.
;;; 
;;; The calculus we use to model components is a simple extension of the
;;; previous calculus with a construction that let us check that the argument of
;;; a message is not a given name, which we write a. FraKtal also provide a way
;;; to bind such a name: ((m) ≠ a) matches a name that is not a and binds it to
;;; m.

(defvar +fraktal+
  (make-instance 'pattern-language
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a␣ρ̅␣
    ;; ξu ::= a␣ρ̅␣↑
    ;; ξd ::= a␣ρ̅␣↓
    ;; ξk ::= a[x]
    ;; ρ ::= a␣ρ̅␣ | ρ|ρ
    ;; ρ̅ ::= x | ρ | (a)␣ρ̅␣ | a̅␣ρ̅␣ | ((m) ≠ a)␣ρ̅␣ | _
    ))

(defclass mismatch (name-type)
  ((complement :initarg :complement :accessor complement)
   (variable :initarg :variable :reader variable)))

(defun != (complement &optional variable)
  (make-instance 'mismatch :variable variable :complement complement))

(defmethod print-object ((obj mismatch) stream)
  (format stream "(!= ~a~@[ ~a~])" (complement obj) (variable obj)))

;;; We similarly extend the matching functions, adding two cases for the helper
;;; function matchr

(defmethod recursive-match ((pattern message) (process message)
                            &optional (substitutions (make-empty-environment)))
  (typecase (name pattern)
    (name-variable (recursive-match (process pattern) (process process)
                                    (unify (name pattern) (name process)
                                           substitutions)))
    (mismatch (let ((name (name pattern)))
                (when (not (eql (complement name) (name process)))
                  (recursive-match (process pattern) (process process)
                                   (if (variable name)
                                     (unify (variable name) (name process)
                                            substitutions)
                                     substitutions)))))
    (otherwise (second (match pattern process substitutions)))))

;; FIXME: This is fraktal-specific, but kind of digs a bit more into the internals than
;;        I'd like
(defmethod replace-name ((name mismatch) mapping &optional ignored-vars)
    (setf (complement name) (replace-name (complement name) mapping ignored-vars))
    name)

(defmethod collect-bound-names ((pattern message))
  (typecase (name pattern)
    (name-variable (cons (name pattern)
                         (collect-bound-names (process pattern))))
    (mismatch (if (variable (name pattern))
                  (cons (variable (name pattern))
                        (collect-bound-names (process pattern)))
                  (collect-bound-names (process pattern))))
    (otherwise (collect-bound-names (process pattern)))))
