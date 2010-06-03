(in-package #:kilns)

;;; This file extends the CL-Unification package with the missing
;;; pieces to allow it to unify terms in the Kell calculus

(defmethod unify
    ((pattern process-variable) agent
     &optional (substitutions (make-empty-environment)))
  (unify (intern (format nil "?~a" (name pattern))) agent substitutions))

;;; FIXME: currently, name variables and process variables can conflict. Do we
;;;        want to be able to have a namevar and procvar with the same name –
;;;        yes, this is important with nested triggers, where a deeper one might
;;;        use a procvar with the same name as a shallower namevar, and they
;;;        can't clash because the types would conflict.
(defmethod unify
    ((pattern name-variable) agent
     &optional (substitutions (make-empty-environment)))
  (unify (intern (format nil "?~a" (name pattern))) agent substitutions))

(defmethod unify
    ((pattern kell) (agent kell) &optional (substitutions (make-empty-environment)))
  (unify (process pattern) (process agent)
         (unify (name pattern) (name agent) substitutions)))

;;; We should only get here if we know that both messages are relative to the same kell
(defmethod unify
    ((pattern message) (agent message)
     &optional (substitutions (make-empty-environment)))
  (unify (process pattern) (process agent)
         (unify (name pattern) (name agent) substitutions)))

(defmethod match-messages (first second)
  (handler-case (unify first second)
    (unification-failure () nil)))

;;; this is probably not necessary, but I don't understand these environments well
(defun duplicate-environment (env)
  (unify::make-environment
   :frames (list (copy-structure (unify::first-frame env)))))

;;; ----------------------------------------------------------------------------
;;; This set of methods (with PATTERN specialized on PATTERN) represents the
;;; top-level match that sends off all the sub-matches, so it works a little
;;; differently. EG, we already know that the pattern has a 1-1 correspondence
;;; with messages.

(defmethod unify
    ((pattern pattern) (agent message)
     &optional (substitutions (make-empty-environment)))
  (unify (car (append (local-message-pattern pattern)
                      (up-message-pattern pattern)
                      (down-message-pattern pattern)))
         agent
         substitutions))

(defmethod unify
    ((pattern pattern) (agent kell) &optional
     (substitutions (make-empty-environment)))
  (unify (car (kell-message-pattern pattern)) agent substitutions))

;; NOTE: FIND-VARIABLE-VALUE isn't generic, so we use a different name
(defmethod find-symbol-value
    ((variable symbol) &optional env errorp)
  (find-variable-value (intern (format nil "?~a" variable)) env errorp))
(defmethod find-process-variable-value
    ((variable process-variable) &optional env errorp)
  (find-variable-value (intern (format nil "?~a" (name variable))) env errorp))
(defmethod find-name-variable-value
    ((variable name-variable) &optional env errorp)
  (find-variable-value (intern (format nil "?~a" (name variable))) env errorp))

;;; It turns out that occurs-in-p is used to make sure that variables don't
;;; match things that contain the same variable - but we don't care, that's
;;; fine in our calculus.
(defmethod unify::occurs-in-p ((var symbol) pat env)
  (declare (ignore env))
  nil)
