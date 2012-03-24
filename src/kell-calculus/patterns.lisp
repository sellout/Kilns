(in-package #:kell-calculus)

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
  ((local-message-pattern :initform nil :initarg :local-message-pattern :type list
                          :accessor local-message-pattern)
   (down-message-pattern :initform nil :initarg :down-message-pattern :type list
                         :accessor down-message-pattern)
   (up-message-pattern :initform nil :initarg :up-message-pattern :type list
                       :accessor up-message-pattern)
   (kell-message-pattern :initform nil :initarg :kell-message-pattern :type list
                         :accessor kell-message-pattern)))

(defmethod print-object ((obj pattern) stream)
  (let ((patterns (append (local-message-pattern obj)
                          (down-message-pattern obj)
                          (up-message-pattern obj)
                          (kell-message-pattern obj))))
    (format stream "~:[~{~a~}~;(par ~{~a~^ ~})~]"
            (< 1 (length patterns)) patterns)))

(defun up (process)
  "Paralleling `cont`, this is used to create an up-pattern with the new
   syntax."
  (setf (continuation process) 'up)
  process)

(defun down (process)
  "Paralleling `cont`, this is used to create a down-pattern with the new
   syntax."
  (setf (continuation process) 'down)
  process)

;;; FIXME: somewhere around here we need to ensure only one kell is in the
;;;        pattern

(defgeneric convert-process-to-pattern (process &optional pattern)
  (:method ((process parallel-composition)
            &optional (pattern (make-instance 'pattern)))
    (mapc (lambda (sub-process)
            (convert-process-to-pattern sub-process pattern))
          (append (messages process) (kells process)))
    pattern)
  (:method ((process message) &optional (pattern (make-instance 'pattern)))
    (unless (argument process) ; FIXME: should only happen with pnp-jK & FraKtal
      (setf (argument process) kilns:_))
    (case (continuation process)
      (down (push process (down-message-pattern pattern)))
      (up (push process (up-message-pattern pattern)))
      (otherwise (push process (local-message-pattern pattern))))
    pattern)
  (:method ((process kell) &optional (pattern (make-instance 'pattern)))
    (push process (kell-message-pattern pattern))
    pattern))

;;; – One can decide whether a pattern matches a given term. More precisely,
;;;   each pattern language is equipped with a decidable relation match, which
;;;   associates a pair ⟨ξ,M⟩, consisting of a pattern ξ and a multiset of
;;;   annotated messages M, with defined substitutions that make the pattern
;;;   match the multiset of annotated messages, if there are such substitutions,
;;;   and with ∅ otherwise (see section 3.2 for more details).
;;;   We write θ ∈ match(ξ, M) for ⟨⟨ξ, M⟩, θ⟩ ∈ match.

(defun compose-hash-tables (&rest hash-tables)
  (let ((combined-hash-table (make-hash-table)))
    (mapc (lambda (hash-table)
            (maphash (lambda (key value)
                       (setf (gethash key combined-hash-table)
                             (append (gethash key combined-hash-table)
                                     value)))
                     hash-table))
          hash-tables)
    combined-hash-table))

(defgeneric match (pattern process &optional substitutions)
  (:method ((pattern pattern) (process list)
            &optional (substitutions (make-empty-environment)))
    (values (match-local (local-message-pattern pattern)
                         process
                         substitutions)))
  (:method ((pattern pattern) (kell kell)
            &optional (substitutions (make-empty-environment)))
    (let ((processes (append (multiple-value-bind (subst procs)
                                 (match-list #'match-local
                                             (local-message-pattern pattern)
                                             (messages kell)
                                             substitutions)
                               (setf substitutions subst)
                               procs)
                             (multiple-value-bind (subst procs)
                                 (match-list #'match-down
                                             (down-message-pattern pattern)
                                             (apply #'compose-hash-tables
                                                    (mapcar #'messages
                                                            (subkells kell)))
                                             substitutions)
                               (setf substitutions subst)
                               procs)
                             (multiple-value-bind (subst procs)
                                 (match-list #'match-up
                                             (up-message-pattern pattern)
                                             (handler-case
                                                 (messages (parent kell))
                                               (unbound-slot () nil))
                                             substitutions)
                               (setf substitutions subst)
                               procs)
                             (multiple-value-bind (subst procs)
                                 (match-list #'match-kell
                                             (kell-message-pattern pattern)
                                             (kells kell)
                                             substitutions)
                               (setf substitutions subst)
                               procs))))
      (values substitutions processes)))
  (:method ((pattern process) (process process)
            &optional (substitutions (make-empty-environment)))
    ;; FIXME: This should ensure that _all_ processes match, not just enough to
    ;;        satisfy pattern.
    (let ((processes (remove nil
                             (append (multiple-value-bind (subst procs)
                                         (match-local (messages-in pattern)
                                                      (messages-in process)
                                                      substitutions)
                                       (setf substitutions subst)
                                       procs)
                                     (multiple-value-bind (subst procs)
                                         (match-kell (kells-in pattern)
                                                     (kells-in process)
                                                     substitutions)
                                       (setf substitutions subst)
                                       procs)))))
      (values substitutions processes)))
  (:method (pattern process &optional (substitutions (make-empty-environment)))
    (values (unify pattern process substitutions)
            process))
  (:method :around
      (pattern process &optional (substitutions (make-empty-environment)))
    ;; FIXME: not really ignorable, but CCL complains
    (declare (ignorable pattern process substitutions))
    (handler-case (call-next-method)
      (unification-failure () (values nil nil)))))

(defun match-list (type-function patterns processes substitutions)
  "Finds one match in PROCESSES for each item in PATTERNS. Also ensures that the
   same process doesn’t match multiple patterns."
  (let* ((matched-processes ())
         (processes
          (mapcar (lambda (pattern)
                    (block per-pattern
                      (mapc (lambda (process)
                              (when (not (find process matched-processes))
                                (let ((subst (funcall type-function
                                                      pattern
                                                      process
                                                      substitutions)))
                                  (when subst
                                    (setf substitutions subst)
                                    (push process matched-processes)
                                    (return-from per-pattern process)))))
                            (gethash (name pattern) processes))
                      (error 'unification-failure
                             :format-control "Could not unify ~s with any process in ~s"
                             :format-arguments (list pattern processes))))
                  patterns)))
    (values substitutions processes)))

(defgeneric match-local (pattern process &optional substitutions)
  (:method ((patterns list) (processes list)
            &optional (substitutions (make-empty-environment)))
    "Finds one match in PROCESSES for each item in PATTERNS. Also ensures that
     the same process doesn’t match multiple patterns."
    (if (= (length patterns) (length processes))
        (let ((processes
               (mapcar (lambda (pattern)
                         (block per-pattern
                           (mapc (lambda (process)
                                   (let ((subst (match-local pattern process
                                                             substitutions)))
                                     (when subst
                                       (setf substitutions subst)
                                       (setf processes (remove process processes))
                                       (return-from per-pattern process))))
                                 processes)
                           (error 'unification-failure
                                  :format-control "Could not unify ~s with any process in ~s"
                                  :format-arguments (list pattern processes))))
                       patterns)))
          (values substitutions processes))
        (error 'unification-failure
               :format-control "Can not unify two different length lists: ~s ~s"
               :format-arguments (list patterns processes)))))
(defgeneric match-down (pattern process &optional substitutions))
(defgeneric match-up (pattern process &optional substitutions))
(defgeneric match-kell (pattern process &optional substitutions)
  (:method ((patterns list) (processes list)
            &optional (substitutions (make-empty-environment)))
    "Finds one match in PROCESSES for each item in PATTERNS. Also ensures that
     the same process doesn’t match multiple patterns."
    (let ((processes
           (mapcar (lambda (pattern)
                     (block per-pattern
                       (mapc (lambda (process)
                               (let ((subst (match-kell pattern process
                                                        substitutions)))
                                 (when subst
                                   (setf substitutions subst)
                                   (setf processes (remove process processes))
                                   (return-from per-pattern process))))
                             processes)
                       (error 'unification-failure
                              :format-control "Could not unify ~s with any process in ~s"
                              :format-arguments (list pattern processes))))
                   patterns)))
      (values substitutions processes))))

;;; – Pattern languages are equipped with three functions fn, bn, and bv, that
;;;   map a pattern ξ to its set of free names, bound name variables, and bound
;;;   process variables, respectively. Note that patterns may have free names,
;;;   but cannot have free process variables (i.e. all process variables
;;;   appearing in a pattern are bound in the pattern).

(defmethod free-names ((pattern pattern))
  (reduce #'union
          (append (local-message-pattern pattern)
                  (down-message-pattern pattern)
                  (up-message-pattern pattern)
                  (kell-message-pattern pattern))
          :key #'free-names))

(defgeneric bound-names (pattern)
  (:documentation
   "This returns a list of all channel-names that are bound by the given pattern."))

(defgeneric bound-variables (pattern)
  (:documentation
   "This returns a list of all process-variables that are bound by the given
    pattern."))

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
