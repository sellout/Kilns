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
  (make-instance 'pattern-language :grammar
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a␣ρ̅␣
    ;; ξu ::= a␣ρ̅␣↑
    ;; ξd ::= a␣ρ̅␣↓
    ;; ξk ::= a[x]
    ;; ρ ::= a␣ρ̅␣ | ρ|ρ
    ;; ρ̅ ::= x | ρ | (a)␣ρ̅␣ | a̅␣ρ̅␣ | ((m) ≠ a)␣ρ̅␣ | _
    ))

(defclass mismatch ()
  ((complement :initarg :complement :reader the-complement)
   (variable :initarg :variable :reader variable)))

;;; We similarly extend the matching functions, adding two cases for the helper
;;; function matchr

(defmethod recursive-match ((language fraktal)
                            (pattern message) (process message))
  (let ((name (name pattern)))
    (if (typep name 'mismatch)
      (when (not (eql (the-complement name) (name process)))
        (when (variable name)
          (bind (variable name) (name process)))
        (recursive-match (process pattern) (process process)))
      (call-next-method))))
