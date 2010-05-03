#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; A polyadic name-passing jK

;;; NOTE: need some way to tell that this is an extension of the jk-calculus
(defvar +pnpjk-calculus+
  (make-instance 'pattern-language :grammar
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a␣ρ̅␣
    ;; ξu ::= a␣ρ̅␣↑
    ;; ξd ::= a␣ρ̅␣↓
    ;; ξk ::= a[x]
    ;; ρ ::= a␣ρ̅␣ | ρ|ρ
    ;; ρ̅ ::= x | ρ | (a)␣ρ̅␣ | _
    ))

;;; In this pattern language, the special pattern _ matches anything.

(defclass blank ()
  ())

(defvar _ (make-instance 'blank))

;;; For convenience, we write a␣x1, ⋯, xn␣ for a␣1␣x1␣ | ⋯ | n␣xn␣␣ where
;;; 1, ⋯, n only occur in these encodings.
;;; 
;;; We also write a␣0␣ for an argument a of a message in processes, and a␣_␣ in
;;; patterns. That is the process
;;; 
;;;     (a␣(b)␣ | c␣k␣ ␣ b␣k␣) | a␣d␣ | c␣k␣
;;; 
;;; corresponds to
;;; 
;;;     (a␣(b)␣_␣␣ | c␣k␣_␣␣ ␣ b␣k␣0␣␣) | a␣d␣0␣␣ | c␣k␣0␣␣
;;; 
;;; The matching functions are easily defined by induction:

(defmethod message-match ((language (eql +pnpjk-calculus+))
                          (pattern local-message) (message message))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message))))

(defmethod message-match ((language (eql +pnpjk-calculus+))
                          (pattern down-message) (message message))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message))))

(defmethod message-match ((language (eql +pnpjk-calculus+))
                          (pattern up-message) (message message))
  (if (equal (name pattern) (name message))
    (recursive-match (process pattern) (process message))))

(defmethod message-match ((language (eql +pnpjk-calculus+))
                          (pattern kell-message) (message kell))
  (if (equal (name pattern) (name message))
    (bind (process pattern) (process message))))

(defgeneric recursive-match (language pattern process)
  (:method ((language (eql +pnpjk-calculus+)) (pattern blank) (process process))
    '())
  (:method ((language (eql +pnpjk-calculus+))
            (pattern process-variable) (process process))
    (bind pattern process))
  (:method ((language (eql +pnpjk-calculus+)) (pattern pattern) (process process))
    (match pattern process))
  (:method ((language (eql +pnpjk-calculus+)) (pattern message) (process message))
    (if (typep (name pattern) 'process-variable)
      (progn
        (bind (name pattern) (name process))
        (recursive-match (process pattern) (process process)))
      (match pattern process)))
