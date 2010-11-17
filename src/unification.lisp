(in-package #:kilns)

;;; This file extends the CL-Unification package with the missing
;;; pieces to allow it to unify terms in the Kell calculus

(defmethod unify
    ((pattern process-variable) agent
     &optional (substitutions (make-empty-environment)))
  (unify (intern (format nil "?~a" (name pattern))) agent substitutions))

(defmethod unify
    ((pattern kell) (agent kell) &optional (substitutions (make-empty-environment)))
  (unify (state pattern) (state agent)
         (unify (name pattern) (name agent) substitutions)))

;;; We should only get here if we know that both messages are relative to the same kell
(defmethod unify
    ((pattern message) (agent message)
     &optional (substitutions (make-empty-environment)))
  (unify (argument pattern) (argument agent)
         (unify (name pattern) (name agent) substitutions)))

(defgeneric name-equal (a b)
  (:method (a b)                   (equal a b))
  (:method ((a symbol) (b symbol)) (equal (symbol-name a) (symbol-name b))))

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
(defun find-symbol-value (variable &optional env errorp)
  (find-variable-value (intern (format nil "?~a" variable)) env errorp))
(defun find-process-variable-value (variable &optional env errorp)
  (find-variable-value (intern (format nil "?~a" (name variable))) env errorp))

;;; It turns out that occurs-in-p is used to make sure that variables don't
;;; match things that contain the same variable - but we don't care, that's
;;; fine in our calculus.
(defmethod unify::occurs-in-p ((var symbol) pat env)
  (declare (ignore pat env))
  nil)

;;; Unification for abstractions and concretions

(defmethod unify
    ((pattern concretion) (agent concretion)
     &optional (substitutions (make-empty-environment)))
  (unify (continuation pattern) (continuation agent)
         (unify (messages pattern) (messages agent)
                (unify (restricted-names pattern) (restricted-names agent)
                       substitutions))))

(defmethod unify
    ((pattern kell-abstraction) (agent kell-abstraction)
     &optional (substitutions (make-empty-environment)))
  (unify (abstraction pattern) (abstraction agent)
         (unify (continuation pattern) (continuation agent)
                (unify (name pattern) (name agent) substitutions))))

(defmethod unify
    ((pattern application-abstraction) (agent application-abstraction)
     &optional (substitutions (make-empty-environment)))
  (unify (abstraction pattern) (abstraction agent)
         (unify (concretion pattern) (concretion agent) substitutions)))

(defmethod unify
    ((pattern restriction-abstraction) (agent restriction-abstraction)
     &optional (substitutions (make-empty-environment)))
  (unify (abstraction pattern) (abstraction agent)
         (unify (names pattern) (names agent) substitutions)))

(defmethod unify
    ((pattern pattern-abstraction) (agent pattern-abstraction)
     &optional (substitutions (make-empty-environment)))
  (unify (process pattern) (process agent)
         (unify (pattern pattern) (pattern agent) substitutions)))
