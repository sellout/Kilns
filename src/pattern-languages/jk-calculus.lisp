#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; To illustrate these definitions, we introduce a first instance of the Kell
;;; calculus with a simple pattern language. We call this calculus jK. The
;;; patterns in jK are defined by the following grammar:

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
    (case (car form)
      ((down up) (values (apply #'define-pattern-message
                                pattern-language (cdr (second form)))
                         (car form)))
      (kell (apply #'define-pattern-kell pattern-language (cdr form)) 'kell)
      (message (values (apply #'define-pattern-message
                              pattern-language (cdr form))
                       'local)))))

(defgeneric define-pattern-message (pattern-language name &rest argument)
  (:method ((pattern-language jk-calculus) name &rest argument)
    (assert (= 1 (length argument)))
    (assert (eq 'process-variable (caar argument)))
    (make-instance 'message
                   :name name
                   :argument (define-pattern-process-variable pattern-language
                                                              (second (car argument))))))

(defgeneric define-pattern-kell (pattern-language name &rest state)
  (:method ((pattern-language jk-calculus) name &rest state)
    (assert (= 1 (length state)))
    (assert (eq 'process-variable (caar state)))
    (make-instance 'kell
                   :name name
                   :state (define-pattern-process-variable pattern-language
                                                           (second (car state))))))

(defgeneric define-pattern-process-variable (pattern-language name)
  (:method ((pattern-language jk-calculus) name)
    (make-instance 'process-variable :name name)))

(defmethod define-pattern ((pattern-language jk-calculus) pattern-form)
  (let ((pattern (make-instance 'pattern)))
    (if (listp pattern-form)
        (flet ((add-process (process type)
                 (case type
                   (down (push process (down-message-pattern pattern)))
                   (up (push process (up-message-pattern pattern)))
                   (local (push process (local-message-pattern pattern)))
                   (kell (push process (kell-message-pattern pattern))))))
          (case (car pattern-form)
            ((down up message kell)
             (multiple-value-call #'add-process
               (define-pattern-process pattern-language pattern-form)))
            (par (mapc (lambda (pattern-process-form)
                         (multiple-value-call #'add-process
                           (define-pattern-process pattern-language
                               pattern-process-form)))
                       (cdr pattern-form)))
            ((cont new trigger var define)
             (error "~A is not a valid pattern form in ~A."
                    pattern-form pattern-language))
            (otherwise (apply #'define-named-concretion pattern-form))))
        (error "Can not use ~A as a pattern form in ~A."
               pattern-form pattern-language))
    (when (< 1 (length (kell-message-pattern pattern)))
      (error "Can only have 1 kell in a pattern in ~A. Invalid pattern: ~A."
             pattern-language pattern))
    pattern))

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
  (append (mapcar #'argument
                  (append (local-message-pattern pattern)
                          (down-message-pattern pattern)
                          (up-message-pattern pattern)))
          (mapcar #'state (kell-message-pattern pattern))))
