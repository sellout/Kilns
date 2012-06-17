#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; To illustrate these definitions, we introduce a first instance of the Kell
;;; calculus with a simple pattern language. We call this calculus jK. The
;;; patterns in jK are defined by the following grammar:

(defclass binding (name-type process)
  ((variable :initform (error "bindings need a variable") :initarg :variable
             :reader variable :type variable))
  (:metaclass contracted-class))

(defmethod print-object ((obj binding) stream)
  (format stream "?~s" (variable obj)))

(defmethod unify
    ((pattern binding) agent
     &optional (substitutions (make-empty-environment))
     &key &allow-other-keys)
  (unify (variable pattern) agent substitutions))

(defclass jk-calculus (pattern-language)
  ())

(defvar +jk-calculus+
  (make-instance 'jk-calculus ;:grammar ()
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a⟨x⟩
    ;; ξu ::= a⟨x⟩↑
    ;; ξd ::= a⟨x⟩↓
    ;; ξk ::= a[x]
    ))

(defmethod egal ((x process-variable) (y process-variable))
  (egal (name x) (name y)))

(defgeneric define-pattern-process (pattern-language form)
  (:documentation "Returns two values, the pattern process, and which category
                  (up, down, local, or kell) it belongs to.")
  (:method ((pattern-language jk-calculus) form)
    (ecase (car form)
      ((down up)
       (let ((message (apply #'define-pattern-message
                             pattern-language (cdr (second form)))))
         (setf (continuation message) (car form))
         message))
      (kell (apply #'define-pattern-kell pattern-language (cdr form)))
      (message (apply #'define-pattern-message pattern-language (cdr form))))))

(defgeneric define-pattern-message (pattern-language name &rest argument)
  (:method ((pattern-language jk-calculus) name &rest argument)
    (make-instance 'message
                   :name (define-name name)
                   :argument (apply #'define-pattern-message-argument
                                    pattern-language argument))))

(defgeneric define-pattern-kell (pattern-language name &rest state)
  (:method ((pattern-language jk-calculus) name &rest state)
    (make-instance 'kell
                   :name (define-name name)
                   :state (apply #'define-pattern-message-argument
                                 pattern-language state))))

(defgeneric define-pattern-process-variable (pattern-language name)
  (:method ((pattern-language jk-calculus) name)
    (make-instance 'binding :variable (make-instance 'process-variable :label name))))

(defgeneric define-pattern-message-argument
    (pattern-language &rest argument-forms)
  (:method-combination contract)
  (:method :require ((pattern-language jk-calculus) &rest argument-forms)
    (= (length argument-forms) 1))
  (:method ((pattern-language jk-calculus) &rest argument-forms)
    (let ((arg (car argument-forms)))
      (if (listp arg)
          (case (car arg)
            (process-variable (define-pattern-process-variable pattern-language
                                  (second arg)))
            (otherwise (apply #'define-pattern-named-concretion
                              pattern-language arg)))
          arg))))

;;; FIXME: This needs to accept _any_ combination of pattern forms, because we
;;;        don't know where in the pattern each of the forms will occur until it
;;;        is expanded.
(defgeneric define-pattern-named-concretion
    (pattern-language name &rest arguments)
  (:method ((pattern-language jk-calculus) name &rest arguments)
    (make-instance 'named-concretion
                   :name name
                   :messages (apply #'order-procs arguments))))

(defmethod define-pattern ((pattern-language jk-calculus) pattern-form)
  (if (listp pattern-form)
      (case (car pattern-form)
        ((down up message kell)
         (define-pattern-process pattern-language pattern-form))
        (par (apply #'parallel-composition
                    (mapcar (lambda (pattern-process-form)
                              (if (listp pattern-process-form)
                                  (define-pattern-process pattern-language
                                      pattern-process-form)
                                  pattern-process-form))
                            (cdr pattern-form))))
        ((cont new trigger pattern-variable define)
         (error "~A is not a valid pattern form in ~A."
                pattern-form pattern-language))
        (otherwise (apply #'define-pattern-named-concretion
                          pattern-language pattern-form)))
      pattern-form))

;;; The matching functions for jK patterns are defined inductively as follows:

(defmethod match-local
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name message))
    (unify (argument pattern) (argument message) substitutions)))

(defmethod match-down
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name message))
    (unify (argument pattern) (argument message) substitutions)))

(defmethod match-up
           ((pattern message) (message message)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name message))
    (unify (argument pattern) (argument message) substitutions)))

(defmethod match-kell
           ((pattern kell) (kell kell)
            &optional (substitutions (make-empty-environment)))
  (if (name-equal (name pattern) (name kell))
    (unify (state pattern) (state kell) substitutions)))

;;; Note that, apart from the use of join patterns (i.e. the possibility to
;;; receive multiple messages at once), the pattern language of jK is extremely
;;; simple and does not allow for name-passing.
;;; 
;;; The sk function for jK patterns is defined as follows:

(defmethod channel-names ((pattern pattern)) ; FIXME need to specialize
  (let ((channels (make-hash-table)))
    (psetf (gethash 'local channels)
           (mapcar #'name (local-message-pattern pattern))
           (gethash 'down channels)
           (mapcar #'name (down-message-pattern pattern))
           (gethash 'up channels)
           (mapcar #'name (up-message-pattern pattern))
           (gethash 'kell channels)
           (mapcar #'name (kell-message-pattern pattern)))
    channels))

;;; The reduction rules R.RED.L and R.RED.G appear formidable, but only because
;;; they allow arbitrary combination of local, up, down and kell messages to be
;;; received by a trigger. Using simple jK patterns, on can see immediately that
;;; the following rules are derived reduction rules in jK:
;;; 
;;; (a⟨x⟩▹P) | a⟨Q⟩.S → P{Q/x} | S [LOCAL]
;;; (a⟨x⟩↓▹P) | b[a⟨Q⟩.S | R].T → P{Q/x} | b[S | R].T [OUT]
;;; b[(a⟨x⟩↑▹P) | R].T | a⟨Q⟩.S → b[P{Q/x} | R].T | S [IN]
;;; (a[x]▹P) | a[Q].S → P{Q/x} | S [KELL]
;;; 
;;; One can notice that the rules LOCAL, OUT, IN, and KELL correspond to the
;;; four kinds of actions discussed in Section 2.

(defmethod bound-names ((pattern pattern))
  '())

(defmethod bound-variables ((pattern pattern))
  (append (mapcar (alexandria:compose #'variable #'argument)
                  (append (local-message-pattern pattern)
                          (down-message-pattern pattern)
                          (up-message-pattern pattern)
                          (mapcar (alexandria:compose #'messages-in #'messages)
                                  (named-concretions pattern))))
          (mapcar (alexandria:compose #'variable #'state)
                  (kell-message-pattern pattern))))
