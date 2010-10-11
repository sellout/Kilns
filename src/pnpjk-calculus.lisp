#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; A polyadic name-passing jK

;;; NOTE: need some way to tell that this is an extension of the jk-calculus
(defvar +pnpjk-calculus+
  (make-instance 'pattern-language
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a⟨ρ̅⟩
    ;; ξu ::= a⟨ρ̅⟩↑
    ;; ξd ::= a⟨ρ̅⟩↓
    ;; ξk ::= a[x]
    ;; ρ ::= a⟨ρ̅⟩ | ρ|ρ
    ;; ρ̅ ::= x | ρ | (a)⟨ρ̅⟩ | _
    ))

;;; In this pattern language, the special pattern _ matches anything.

(defclass blank (process)
  ())

(defvar _ (make-instance 'blank))

(defmethod print-object ((obj blank) stream)
  (format stream "_"))

;;; For convenience, we write a␣x1, ⋯, xn␣ for a␣1␣x1␣ | ⋯ | n␣xn␣␣ where
;;; 1, ⋯, n only occur in these encodings.
;;; 
;;; We also write a␣0␣ for an argument a of a message in processes, and a␣_␣ in
;;; patterns. That is the process
;;; 
;;;     (a⟨(b)⟩ | c⟨k⟩ ␣ b⟨k⟩) | a⟨d⟩ | c⟨k⟩
;;; 
;;; corresponds to
;;; 
;;;     (a⟨(b)⟨_⟩⟩ | c⟨k⟨_⟩⟩ ␣ b⟨k⟨0⟩⟩) | a⟨d⟨0⟩⟩ | c⟨k⟨0⟩⟩
;;; 
;;; The matching functions are easily defined by induction:

;;; FIXME: the matching functions are defined as :AROUND to silence
;;;        duplicate definition warnings. Once pattern languages are
;;;        modular, the :AROUND should go away.

(defmethod match-local :around
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message) substitutions)))

(defmethod match-down :around
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message) substitutions)))

(defmethod match-up :around
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message) substitutions)))

(defmethod match-kell :around
           ((pattern kell) (kell kell)
            &optional (substitutions (make-empty-environment)))
  (if (equal (name pattern) (name kell))
    (unify (process pattern) (process kell) substitutions)))

(defgeneric recursive-match (pattern process &optional substitutions)
  (:method (pattern process &optional (substitutions (make-empty-environment)))
    (unify pattern process substitutions))
  (:method ((pattern null-process) (process null-process)
            &optional (substitutions (make-empty-environment)))
    substitutions)
  (:method ((pattern blank) process
            &optional (substitutions (make-empty-environment)))
    (declare (ignore process))
    substitutions)
  (:method ((pattern process-variable) process
            &optional (substitutions (make-empty-environment)))
    (unify pattern process substitutions))
  (:method ((pattern process) process
            &optional (substitutions (make-empty-environment)))
    (second (match pattern process substitutions)))
  (:method ((pattern message) (process message)
            &optional (substitutions (make-empty-environment)))
    (if (typep (name pattern) 'name-variable)
      (recursive-match (process pattern) (process process)
                       (unify (name pattern) (name process) substitutions))
      (second (match pattern process substitutions)))))

(defgeneric collect-bound-names (pattern)
  (:method (pattern)
    (declare (ignore pattern))
    '())
  (:method ((pattern message))
    (if (typep (name pattern) 'name-variable)
      (cons (name pattern) (collect-bound-names (process pattern)))
      (collect-bound-names (process pattern))))
  (:method ((pattern parallel-composition))
    (mapcan #'collect-bound-names (messages pattern))))

(defmethod bound-names :around ((pattern pattern))
  (mapcan #'collect-bound-names
          (append (local-message-pattern pattern)
                  (down-message-pattern pattern)
                  (up-message-pattern pattern))))

(defgeneric collect-bound-variables (pattern)
  (:method (pattern)
    (declare (ignore pattern))
    nil)
  (:method ((pattern process-variable))
    (list pattern))
  (:method ((pattern message))
    (collect-bound-variables (process pattern)))
  (:method ((pattern kell))
    (list (process pattern)))
  (:method ((pattern parallel-composition))
    (mapcan #'collect-bound-variables (messages pattern))))

(defmethod bound-variables :around ((pattern pattern))
  (mapcan #'collect-bound-variables
          (append (local-message-pattern pattern)
                  (down-message-pattern pattern)
                  (up-message-pattern pattern)
                  (kell-message-pattern pattern))))
