;;; The syntax of the Kell calculus is given in Figure 1. It is parameterized by
;;; the pattern language used to define patterns ξ in triggers ξ ␣ P.

;;; P ::= 0 | x | ξ␣P | νa.P | a␣P␣.P | P|P | a[P].P a∈N, x∈V

;;; Names and Variables
;;; 
;;; We assume an infinite set N of names, and an infinite set V of process
;;; variables. We assume that N ∩ V = ∅. We let a, b, n, m and their decorated
;;; variants range over N; and p, q, x, y range over V. The set L of identifiers
;;; is defined as L = N ∪ V.
;;; 
;;; Processes
;;; 
;;; Terms in the Kell calculus grammar are called processes.

(defclass process ()
  ())

;;; We note K L the set of Kell calculus processes with patterns in pattern
;;; language L. In most cases the pattern language used is obvious from the
;;; context, and we simply write K. We let P, Q, R, S, T and their decorated
;;; variants range over processes. We call message a process of the form a␣P␣.Q.

(defclass message (process)
  ((name :type name)
   (process :type process)
   (continuation :type process)))

;;; We let M, N and their decorated variants range over messages and parallel
;;; composition of messages. We call kell a process of the form a[P].Q.

(defclass kell (process)
  ((name :type name)
   (process :type process)
   (continuation :type process)
   ;; implementation details
   (messages :initform (make-hash-table :test #'equal))
   (local-patterns :initform (make-hash-table :test #'equal))
   (up-patterns :initform (make-hash-table :test #'equal))
   (down-patterns :initform (make-hash-table :test #'equal))
   (kell-patterns :initform (make-hash-table :test #'equal))))

;;; The name a in a kell a[P ].Q is called the name of the kell. In a kell of
;;; the form a[… | aj[Pj] | …] we call subkells the processes aj[Pj].

;;; Abbreviations and conventions
;;; 
;;; We abbreviate a␣P␣ a message of the form a␣P␣.0. We abbreviate a a message
;;; of the form a␣0␣. We abbreviate a[P] a kell of the form a[P].0. In a term
;;; νa.P, the scope extends as far to the right as possible. In a term ξ␣P, the
;;; scope of ␣ extends as far to the left and to the right as possible. Thus,
;;; a␣c␣ | b[y] ␣ P | Q stands for (a␣c␣ | b[y])␣(P | Q). We use standard
;;; abbreviations from the the π-calculus: νa1…aq.P for νa1.…νaq.P, or νã.P if
;;; ã = (a1…aq). By convention, if the name vector ã is null, then νã.P =∆ P.
;;; Also, we abuse notation and note ã the set {a1, …, an}, where ã is the
;;; vector a1…an. We note ∏j∈J Pj, J = {1, …, n} the parallel composition
;;; (P1 | (… (Pn−1 | Pn) …)). By convention, if J = ∅, then ∏j∈J Pj =∆ 0.
;;; 
;;; For the definition of the operational semantics of the calculus, we use
;;; additional terms called annotated messages. Annotated messages comprise:
;;; – Local messages: a local message is a term of the form a␣P␣. We write Mm
;;;   for a multiset of local messages.
;;; – Up messages: an up message is a term of the form a␣P␣↑b. We write Mu for
;;;   a multiset of up messages.
;;; – Down messages: a down message is a term of the form a␣P␣↓b. We write Md
;;;   for a multiset of down messages.
;;; – Kell messages: a kell message is a term of the form a[P]. We write Mk for
;;;   a multiset of kell messages.
;;; 
;;; We write M for a multiset of annotated messages. Some of these terms are not
;;; processes, namely those in Mu and Md; they are only used for matching
;;; purposes. We often write these multisets as parallel compositions of
;;; annotated messages.
;;; 
;;; Let Mm be a multiset of local messages. We write Mm↑b for the multiset of up
;;; messages {m↑b | m ∈ Mm}, and Mm↓b for the multiset of down messages
;;; {m↓b | m ∈ Mm}.
;;; 
;;; Let M = {mjnj | j ∈ J} be an arbitrary multiset (where the multiplicity of
;;; element mj is nj). We note M.supp = {mj | j ∈ J} the support set of M, i.e.
;;; the smallest set to which elements of M belong.
