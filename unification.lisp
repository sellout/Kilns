(in-package #:kilns)

;;; This file extends the CL-Unification package with the missing
;;; pieces to allow it to unify terms in the Kell calculus

(defmethod unify
    ((pattern process-variable) agent
     &optional (substitutions (make-empty-environment)))
  (warn "unifying ~a and ~a with ~a" pattern agent substitutions)
  (unify (intern (format nil "?~a" (name pattern))) agent substitutions))

(defmethod unify
    ((pattern kell) (agent kell) &optional (substitutions (make-empty-environment)))
  (unify (process pattern) (process agent)
         (unify (name pattern) (name agent) substitutions)))

;;; We should only get here if we know that both messages are relative to the same kell
(defmethod unify
    ((pattern message) (agent message)
     &optional (substitutions (make-empty-environment)))
  (warn "unifying ~a and ~a with ~a" pattern agent substitutions)
  (unify (process pattern) (process agent)
         (unify (name pattern) (name agent) substitutions)))

(defmethod match-messages (first second)
  (handler-case (unify first second)
    (unification-failure () nil)))

;;; this is probably not necessary, but I don't understand these environments well
(defun duplicate-environment (env)
  (unify::make-environment
   :frames (list (copy-structure (unify::first-frame env)))))

;;; -----------------------------------------------------------------------------------
;;; This set of methods (with PATTERN specialized on PATTERN) represents the top-level
;;; match that sends off all the sub-matches, so it works a little differently. EG, we
;;; already know that the pattern has a 1-1 correspondence with messages.

(defmethod unify
    ((pattern pattern) (agent message)
     &optional (substitutions (make-empty-environment)))
  (unify (car (append (local-message-pattern pattern)
                      (up-message-pattern pattern)
                      (down-message-pattern pattern)))
         agent
         substitutions))

(defmethod unify
    ((pattern pattern) (agent kell) &optional (substitutions (make-empty-environment)))
  (unify (car (kell-message-pattern pattern)) agent substitutions))

;; NOTE: FIND-VARIABLE-VALUE isn't generic, so we use a different name
(defmethod find-process-variable-value
    ((variable process-variable) &optional env errorp)
  (find-variable-value (intern (format nil "?~a" (name variable))) env errorp))

#| FIXME: this is specific to Fraktal
(defmethod unify
    ((pattern mismatch) agent
     &optional (substitutions (make-empty-environment)))
  (if (eql (the-complement pattern) agent)
      (error 'unification-failure
         :format-control "could not unify ~a and ~a"
         :format-arguments (list pattern agent))
      (unify (variable pattern) agent substitutions)))
|#

#|
(defmethod unify
    ((pattern pattern) (agent list)
     &optional (substitutions (make-empty-environment)))
     )
|#

(defmethod unify::occurs-in-p ((var symbol) (pat null-process) env)
  nil)

(defmethod unify::occurs-in-p ((var symbol) (pat message) env)
  (or (unify::occurs-in-p var (name pat) env)
      (unify::occurs-in-p var (process pat) env)))

(defmethod unify::occurs-in-p ((var symbol) (pat kell) env)
  (or (unify::occurs-in-p var (name pat) env)
      (unify::occurs-in-p var (process pat) env)))

#|
(defmethod unify::occurs-in-p ((var symbol) (pat parallel) env)
  (unify::occurs-in-p var (list-from pat) env))
|#