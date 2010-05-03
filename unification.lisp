(in-package #:core-kell)

;;; This file extends the CL-Unification package with the missing
;;; pieces to allow it to unify terms in the Kell calculus

(defmethod unify
    ((pattern process-variable) agent
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

(defmethod unify
    ((pattern parallel-composition) (agent parallel-composition)
     &optional (substitutions (make-empty-environment)))
  (let ((patterns (list-from pattern))
        (agents (list-from agent)))
    (dolist (process agents)
      (handler-case
          (return-from unify
            (unify (make-parallel (cdr patterns))
                   (make-parallel (remove process agents
                                          :test #'match-messages :count 1))
                   (unify (car patterns) process
                          (duplicate-environment substitutions))))
        (unification-failure ())))
    (error 'unification-failure
           :format-control "could not unify ~a and ~a"
           :format-arguments (list pattern agent))))

(defmethod unify
    ((pattern mismatch) agent
     &optional (substitutions (make-empty-environment)))
  (if (eql (the-complement pattern) agent)
      (error 'unification-failure
         :format-control "could not unify ~a and ~a"
         :format-arguments (list pattern agent))
      (unify (variable pattern) agent substitutions)))

(defmethod unify
    ((pattern pattern) (agent list)
     &optional (substitutions (make-empty-environment)))
     )


(defmethod unify::occurs-in-p ((var symbol) (pat annotated-message) env)
  (or (unify::occurs-in-p var (name pat) env)
      (unify::occurs-in-p var (process pat) env)))

(defmethod unify::occurs-in-p ((var symbol) (pat parallel) env)
  (unify::occurs-in-p var (list-from pat) env))