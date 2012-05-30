#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; A polyadic name-passing jK

(defclass pnpjk-calculus (jk-calculus)
  ())

(defvar +pnpjk-calculus+
  (make-instance 'pnpjk-calculus
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

(defclass name-variable (name-type)
  ((name :initarg :name :type symbol :reader name))
  (:documentation
   "These only exist in “potential” processes. When a trigger is triggered, we
    convert each name-variable into its “realized” name and do so
    recursively through nested processes, except where the variable is shadowed
    by a more local variable with the same name."))

(defmethod print-object ((obj name-variable) stream)
  (format stream "?~s" (name obj)))

(defmethod egal ((x name-variable) (y name-variable))
  (egal (name x) (name y)))

;;; FIXME: currently, name variables and process variables can conflict. Do we
;;;        want to be able to have a namevar and procvar with the same name –
;;;        yes, this is important with nested triggers, where a deeper one might
;;;        use a procvar with the same name as a shallower namevar, and they
;;;        can't clash because the types would conflict.
(defmethod unify
    ((pattern name-variable) agent
     &optional (substitutions (make-empty-environment))
     &key &allow-other-keys)
  (unify (intern (format nil "?~a" (name pattern))) agent substitutions))

(defmethod kell-calculus::apply-restriction
    (local-names global-names (process name-variable))
  (multiple-value-bind (new-name replacedp)
      (kell-calculus::apply-restriction local-names global-names (name process))
    (values (if replacedp
                (make-instance 'name-variable :name new-name)
                process)
            replacedp)))

(defun find-name-variable-value (variable &optional env errorp)
  (find-variable-value (intern (format nil "?~a" (name variable))) env errorp))

(defclass blank (process)
  ())

(defvar +blank+ (make-instance 'blank))

(defmethod print-object ((obj blank) stream)
  (format stream "_"))

(defmethod egal ((x blank) (y blank))
  t)

(defmethod unify
    ((pattern blank) agent
     &optional (substitutions (make-empty-environment))
     &key &allow-other-keys)
  "Always matches."
  (declare (ignore agent))
  substitutions)

(defgeneric define-pattern-name-variable (pattern-language name)
  (:method ((pattern-language pnpjk-calculus) name)
    (make-instance 'name-variable :name name)))

(defgeneric define-pattern-nested-message
    (pattern-language name &rest argument-forms)
  (:method ((pattern-language pnpjk-calculus) name &rest argument-forms)
    (make-instance 'message
                   :name (if (listp name)
                             (define-pattern-name-variable pattern-language
                                                           (second name))
                             name)
                   :argument (apply #'define-pattern-message-argument
                                    pattern-language argument-forms))))

(defmethod define-pattern-message-argument :require
    ((pattern-language pnpjk-calculus) &rest argument-forms)
  (declare (ignore argument-forms))
  t) ; just eliminate the requirement from the jk-calculus

(defmethod define-pattern-message-argument
    ((pattern-language pnpjk-calculus) &rest argument-forms)
  (if (null argument-forms)
      +blank+
      (reduce #'compose
              (mapcar (lambda (process-form)
                        (if (listp process-form)
                            (case (car process-form)
                              (process-variable (define-pattern-process-variable pattern-language
                                                    (second process-form)))
                              (par (mapcar (lambda (process-form)
                                             (apply #'define-pattern-message
                                                    pattern-language (cdr process-form)))
                                           (cdr process-form)))
                              (message (apply #'define-pattern-nested-message
                                              pattern-language (cdr process-form)))
                              (otherwise (apply #'define-pattern-named-concretion
                                                pattern-language process-form)))
                            (if (eq process-form '_)
                                +blank+
                                process-form)))
                      argument-forms))))

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
  (if (name-equal (name pattern) (name message))
    (recursive-match (argument pattern) (argument message) substitutions)))

(defmethod match-down :around
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name message))
    (recursive-match (argument pattern) (argument message) substitutions)))

(defmethod match-up :around
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name message))
    (recursive-match (argument pattern) (argument message) substitutions)))

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
    (match pattern process substitutions))
  (:method ((pattern message) (process message)
            &optional (substitutions (make-empty-environment)))
    (if (typep (name pattern) 'name-variable)
      (recursive-match (argument pattern) (argument process)
                       (unify (name pattern) (name process) substitutions))
      (match pattern process substitutions))))

(defgeneric collect-bound-names (pattern)
  (:method (pattern)
    (declare (ignore pattern))
    '())
  (:method ((pattern message))
    (if (typep (name pattern) 'name-variable)
      (cons (name pattern) (collect-bound-names (argument pattern)))
      (collect-bound-names (argument pattern))))
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
    (collect-bound-variables (argument pattern)))
  (:method ((pattern kell))
    (list (state pattern)))
  (:method ((pattern parallel-composition))
    (mapcan #'collect-bound-variables (messages pattern))))

(defmethod bound-variables :around ((pattern pattern))
  (mapcan #'collect-bound-variables
          (append (local-message-pattern pattern)
                  (down-message-pattern pattern)
                  (up-message-pattern pattern)
                  (kell-message-pattern pattern))))
