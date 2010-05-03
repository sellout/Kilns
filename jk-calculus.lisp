#+xcvb (module (:depends-on ("syntax")))
(in-package :kilns)

;;; To illustrate these definitions, we introduce a first instance of the Kell
;;; calculus with a simple pattern language. We call this calculus jK. The
;;; patterns in jK are defined by the following grammar:

(defvar +jk-calculus+
  (make-instance 'pattern-language ;:grammar ()
    ;; ξ ::= J | ξk | J|ξk
    ;; J ::= ξm | ξd | ξu | J|J
    ;; ξm ::= a␣x␣
    ;; ξu ::= a␣x␣↑
    ;; ξd ::= a␣x␣↓
    ;; ξk ::= a[x]
    ))

;;; The matching functions for jK patterns are defined inductively as follows:

(defmethod match-local ((pattern message) (message message) &optional pattern-language)
  (if (equal (name pattern) (name message))
    (unify (process pattern) (process message))))

(defmethod match-down ((pattern message) (message message) &optional pattern-language)
  (if (equal (name pattern) (name message))
    (unify (process pattern) (process message))))

(defmethod match-up ((pattern message) (message message) &optional pattern-language)
  (if (equal (name pattern) (name message))
    (unify (process pattern) (process message))))

(defmethod match-kell ((pattern message) (kell kell) &optional pattern-language)
  (if (equal (name pattern) (name kell))
    (unify (process pattern) (process kell))))

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
;;; (a␣x␣␣P) | a␣Q␣.S → P{Q/x} | S [LOCAL]
;;; (a␣x␣↓ ␣P) | b[a␣Q␣.S | R].T → P{Q/x} | b[S | R].T [OUT]
;;; b[(a␣x␣↑ ␣P) | R].T | a␣Q␣.S → b[P{Q/x} | R].T | S [IN]
;;; (a[x]␣P) | a[Q].S → P{Q/x} | S [KELL]
;;; 
;;; One can notice that the rules LOCAL, OUT, IN, and KELL correspond to the
;;; four kinds of actions discussed in Section 2.
