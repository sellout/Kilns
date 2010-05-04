(in-package #:kilns)

(defclass pattern-language ()
  (grammar :initarg :grammar))

(defvar *current-pattern-language* nil
  "This is the currently active pattern language.")

;;; A pattern ξ is an element of a pattern language L. A pattern ξ acts as a
;;; binder in the calculus. A pattern can bind name variables, of the form (a),
;;; where a ∈ N, and process variables. All name and process variables appearing
;;; in a pattern ξ are bound by the pattern. Name variables can only match
;;; names. Process variables can only match processes. Patterns are supposed to
;;; be linear with respect to process variables, that is, each process variable
;;; x occurs only once in a given pattern ξ.
;;;
;;; We make the following assumptions on a pattern language L:
;;; – A pattern language L is a set of patterns that are multisets of single
;;;   patterns ξm, ξd, ξu, and ξk. The language L can be described by a
;;;   grammar, with the multiset union being represented by parallel
;;;   composition.
;;;   • ξm is taken from the set Ξm and is a local message pattern: it is used
;;;     to match local messages;
;;;   • ξd is taken from the set Ξd and is a down message pattern: it is used
;;;     to match messages from immediate subkells;
;;;   • ξu is taken from the set Ξu and is a up message pattern: it is used to
;;;     match messages from the environment of the enclosing kell;
;;;   • ξk is taken from the set Ξk and is a kell message pattern: it is used
;;;     to match immediate subkells.

(defclass pattern ()
  ((local-message-pattern :initform nil :type list :accessor local-message-pattern)
   (down-message-pattern :initform nil :type list :accessor down-message-pattern)
   (up-message-pattern :initform nil :type list :accessor up-message-pattern)
   (kell-message-pattern :initform nil :type list :accessor kell-message-pattern)))

(defmethod print-object ((obj pattern) stream)
  (format stream "~{~a~^ | ~}"
          (remove-if (lambda (string) (= (length string) 0))
                     (list (format nil "~{~a~^|~}" (local-message-pattern obj))
                           (format nil "~{~a↓~^|~}" (down-message-pattern obj))
                           (format nil "~{~a↑~^|~}" (up-message-pattern obj))
                           (format nil "~{~a~^|~}" (kell-message-pattern obj))))))

(defun message-pattern (name &optional process)
  (let ((pattern (make-instance 'pattern)))
    (push (message name process) (local-message-pattern pattern))
    pattern))
(defun down-pattern (name &optional process)
  (let ((pattern (make-instance 'pattern)))
    (push (message name process) (down-message-pattern pattern))
    pattern))
(defun up-pattern (name &optional process)
  (let ((pattern (make-instance 'pattern)))
    (push (message name process) (up-message-pattern pattern))
    pattern))
(defun kell-pattern (name &optional process)
  (let ((pattern (make-instance 'pattern)))
    (push (kell name process) (kell-message-pattern pattern))
    pattern))
(defun pattern-composition (&rest patterns)
  (reduce #'compose-patterns patterns))

(defun compose-patterns (pattern-a pattern-b)
  (psetf (local-message-pattern pattern-a)
         (nconc (local-message-pattern pattern-a)
                (local-message-pattern pattern-b))
         (down-message-pattern pattern-a)
         (nconc (down-message-pattern pattern-a)
                (down-message-pattern pattern-b))
         (up-message-pattern pattern-a)
         (nconc (up-message-pattern pattern-a)
                (up-message-pattern pattern-b))
         (kell-message-pattern pattern-a)
         (nconc (kell-message-pattern pattern-a)
                (kell-message-pattern pattern-b)))
  pattern-a)

;;; – One can decide whether a pattern matches a given term. More precisely,
;;;   each pattern language is equipped with a decidable relation match, which
;;;   associates a pair ␣ξ,M␣, consisting of a pattern ξ and a multiset of
;;;   annotated messages M, with defined substitutions that make the pattern
;;;   match the multiset of annotated messages, if there are such substitutions,
;;;   and with ∅ otherwise (see section 3.2 for more details).
;;;   We write θ ∈ match(ξ, M) for ␣␣ξ, M ␣, θ␣ ∈ match.

;;; FIXME: need to sort all this out with some cl-unification

(defun match
    (pattern kell &optional (pattern-language *current-pattern-language*))
  (match-local (local-message-pattern pattern) (messages-in (process kell))
               pattern-language)
  (match-down (down-message-pattern pattern)
              (messages-in (process (subkells kell)))
              pattern-language)
  (match-up (up-message-pattern pattern) (messages-in (process (parent kell)))
            pattern-language)
  (match-kell (kell-message-pattern pattern) (kells-in (process kell))
              pattern-language)
  ;;(reduce #'union
  ;;        (mapcar (lambda (message-set)
  ;;                  (dotimes (j (length patterns))
  ;;                    (message-match *current-pattern-language*
  ;;                                   (aref patterns j)
  ;;                                   (aref message-set j))))
  ;;                (permute annotated-messages)))
  )

(defgeneric match-local (pattern process &optional pattern-language))
(defgeneric match-down (pattern process &optional pattern-language))
(defgeneric match-up (pattern process &optional pattern-language))
(defgeneric match-kell (pattern process &optional pattern-language))

;;; – Pattern languages are equipped with three functions fn, bn, and bv, that
;;;   map a pattern ξ to its set of free names, bound name variables, and bound
;;;   process variables, respectively. Note that patterns may have free names,
;;;   but cannot have free process variables (i.e. all process variables
;;;   appearing in a pattern are bound in the pattern).

(defgeneric bound-names (process)
  )

(defgeneric bound-variables (process)
  )

(defgeneric free-names (process)
  (:documentation "Process -> {Name}")
  (:method ((process null-process))
    '())
  ;;  (:method ((process name))
  ;;    (list process))
  (:method ((process process-variable))
    '())
  (:method ((process restriction))
    (set-difference (free-names (process process)) (list (name process))))
  (:method ((process kell))
    (reduce #'union
            (list (free-names (name process))
                  (free-names (process process))
                  (free-names (continuation process)))))
  (:method ((process message))
    (reduce #'union
            (list (free-names (name process))
                  (free-names (process process))
                  (free-names (continuation process)))))
  (:method ((process parallel-composition))
    (reduce #'union (map-parallel-composition #'free-names process)))
  (:method ((process trigger))
    (union (free-names (pattern process))
           (set-difference (free-names (process process))
                           (bound-names (pattern process))))))

;;; – Pattern languages are equipped with a function sk, which maps a pattern ξ
;;;   to a multiset of names. Intuitively, ξ.sk corresponds to the multiset of
;;;   channel names on which pattern ξ expects messages or kells (we use
;;;   indifferently an infix or a postfix notation for sk). We identify
;;;   ξ.sk = {ai | i ∈ I} with the action ∏ i∈I ai (see section 3.3 for more
;;;   details). By definition, we set ξ.sk.supp ⊆ fn(ξ). In other terms, a
;;;   pattern may not bind a name that appears in its set of channel names (a
;;;   trigger must know channel names in order to receive messages on them).

(defgeneric channel-names (pattern)
  (:documentation "Returns a hash-table with 4 entries - one each for local, up,
                   down, and kell names."))

;;; – Pattern languages are equipped with a structural congruence relation
;;;   between patterns, noted ≡. We assume the following properties: if ξ ≡ ζ,
;;;   then fn(ξ) = fn(ζ), ξ.sk = ζ.sk, and bn(ξ) ∪ bv(ξ) = bn(ζ) ∪ bv(ζ).
;;;   Moreover, the interpretation of join patterns as multisets of simple
;;;   patterns implies that the structural congruence on patterns must include
;;;   the associativity and commutativity of the parallel composition operator.
;;; – A pattern language is compatible with the structural congruence defined
;;;   below (see section 3.2 for more details), in particular if P ≡ Q then
;;;   there is no Kell calculus context that can distinguish between P and Q.

(defmethod structurally-congruent ((left pattern) (right pattern))
  (and (set= (free-names left) (free-names right))
       (set= (channel-names left) (channel-names right))
       (set= (union (bound-names left) (bound-variables left))
             (union (bound-names right) (bound-variables right)))))
