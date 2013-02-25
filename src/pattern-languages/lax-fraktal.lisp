(in-package :kilns)

;;; This is a version of fraktal that allows for more complex kell patterns.
;;; It’s needed for the current definition of the lambda calculus. Hopefully,
;;; we’ll find a definition that work with FraKtal and this can go away.

(defclass lax-fraktal (fraktal)
  ())

(defvar +lax-fraktal+
  (make-instance 'lax-fraktal))

(defmethod collect-bound-variables ((pattern-language lax-fraktal) (pattern kell))
  (collect-bound-variables pattern-language (state pattern)))
