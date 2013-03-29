;;;(defpackage kiln-runner
;;;  (:use #:cl #:kilns)
;;;  (:shadow #:load #:read #:eval)
;;;  (:export #:load #:read #:eval))

(in-package #:kilns)

(defvar *top-kell*)
(defvar *local-kell*)

(defun get-ordered-parameters (process)
  (mapcar (alexandria:compose #'translate-form #'argument)
          (sort (copy-list (kell-calculus::messages-in process))
                #'<
                :key (alexandria:compose #'label #'name))))

(defun translate-form (form)
  (if (typep form 'named-concretion)
      (let ((new-form (expand-concretions form)))
        (if (eq new-form form)
            `(,(translate-form (name form))
               ,@(get-ordered-parameters (messages form)))
            new-form))
      form))

(defvar *global-definitions* (make-hash-table :test #'eq)
  "The set of definitions that are available anywhere in the system.")

(defmacro defglobal (name (&rest arguments) aggro-expand &body body)
  "This allows us to add global built-ins to Kilns."
  `(setf (gethash ',(intern (symbol-name name) :kilns-user)
                  *global-definitions*)
         (lambda (concretion)
           (destructuring-bind (,@arguments)
               (get-ordered-parameters (let ((kell-calculus::*force-resolution-p* ,aggro-expand))
                                         (kell-calculus::resolve-placeholders
                                          (messages concretion)
                                          (kell-calculus::lexical-names concretion)
                                          (kell-calculus::lexical-placeholders concretion)
                                          (kell-calculus::suspended-values concretion))))
             ,@body))))

(defvar *delayed-concretions* (make-hash-table :test #'eq)
  "Concretions that are in matchable space, but for which there is not yet any
   definition to expand them. They need to be expanded as soon as such a
   definition appears.")

(define-condition kiln-error (error)
  ()
  (:documentation "This is used to distinguish errors that happen in kilns from
                   errors in the implementation."))

(define-condition duplicate-kell-name-error (kiln-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream
                     "Can't have two kells with the same name (~a) in the same ~
                      kell."
                     (slot-value condition 'name)))))

(define-condition no-such-kell-error (kiln-error)
  ((name :initarg :name)
   (container :initarg :container))
  (:report (lambda (condition stream)
             (format stream "No kell named ~a within ~a."
                     (slot-value condition 'name)
                     (slot-value condition 'container)))))

(define-condition no-such-variable-error (kiln-error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "No process variable named ~a in the current scope."
                     (slot-value condition 'name)))))

(let ((lock (make-lock "print-lock")))
  (defun printk (&rest arguments)
    "This is our log-to-screen function. Works like format, but makes sure
     messages are printed atomically. It also starts each new message on its own
     line (since there's no guarantee that the previous message is even from the
     same thread, this is totally reasonable)."
    (with-lock-held (lock)
      (apply #'format t "~&~@?" arguments)
      (finish-output))))

;;; FIXME: make sure these threadsafe functions truly are

(defvar *new-events* (make-condition-variable))
(defvar *event-queue* '())
(defvar *dummy-wait-lock* (make-lock "dummy-wait-lock"))

(let ((lock (make-lock "event-lock")))
  (defun pop-event ()
    "THIS NEEDS TO BE THREADSAFE"
    (with-lock-held (lock)
      (pop *event-queue*)))

  (defun push-event (item)
    "THIS NEEDS TO BE THREADSAFE"
    (with-lock-held (lock)
      (setf *event-queue* (append *event-queue* (list item))))
    (condition-notify *new-events*))

  (defun clear-events ()
    "THIS NEEDS TO BE THREADSAFE"
    (with-lock-held (lock)
      (setf *event-queue* '())))

  (defun remove-events-for-kell (kell)
    (with-lock-held (lock)
      (setf *event-queue* (delete kell *event-queue* :key #'second)))))

(defparameter *debugp* nil
  "If T, errors will dump you to the debugger (although you still need to do
   some work to be in the correct thread to _use_ the debugger).")

(defun handle-error (c)
  (if *debugp*
      (error c)
      (printk "ERROR: ~a~%" c)))

(defun run-kiln ()
  (loop do (handler-case (let ((event (pop-event)))
                           (if event
                               (apply #'match-on event)
                               (with-lock-held (*dummy-wait-lock*)
                                 (condition-wait *new-events*
                                                 *dummy-wait-lock*))))
             (kiln-error (c) (handle-error c)))))

(defun start-kilns (count)
  (loop for i from 1 to count
    collecting (make-thread #'run-kiln :name (format nil "kiln ~d" i))))

(defgeneric collect-channel-names (process kell)
  (:documentation "This returns a list of events to add to the event queue.")
  (:method-combination contract)
  (:method :guarantee "returns a list of (process kell) lists" (process kell)
     (declare (ignore process kell))
     (and (= 1 (length (multiple-value-list (results))))
          (every (lambda (list)
                   (and (typep (first list) '(or message kell trigger))
                        (typep (second list) 'kell)))
                 (results))))
  (:method ((process parallel-composition) (kell kell))
    ;; FIXME: I don't think this ever gets called
    (apply #'append
           (map-parallel-composition (lambda (proc)
                                       (collect-channel-names proc kell))
                                     process)))
  (:method ((process message) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (messages kell)))
      (list (list process kell))))
  (:method ((process kell) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (kells kell)))
      (list (list process kell))))
  (:method ((process pattern-abstraction) (kell kell))
    (mapc (lambda (pattern)
            (push process (gethash (name pattern) (local-patterns kell))))
          (local-message-pattern (pattern process)))
    (mapc (lambda (pattern)
            (push process (gethash (name pattern) (down-patterns kell))))
          (down-message-pattern (pattern process)))
    (mapc (lambda (pattern)
            (push process (gethash (name pattern) (up-patterns kell))))
          (up-message-pattern (pattern process)))
    (mapc (lambda (pattern)
            (push process (gethash (name pattern) (kell-patterns kell))))
          (kell-message-pattern (pattern process)))
    (list (list process kell)))
  (:method ((process null-process) (kell kell))
    (declare (ignore kell))
    '()))

(defgeneric activate-process (process kell)
  (:method ((process process) (kell kell))
    (setf (parent process) kell)
    (mapc #'push-event (collect-channel-names process kell)))
  (:method ((process kell) (kell kell))
    (call-next-method)
    (activate-process (state process) process))
  (:method ((process parallel-composition) (kell kell))
    (kell-calculus::map-parallel-composition (alexandria:rcurry #'activate-process kell)
                                             process))
  (:method ((process pattern-abstraction) (kell kell))
    (setf (parent process) kell)
    (mapc #'push-event (collect-channel-names process kell)))
  (:method ((process restriction-abstraction) (kell kell))
    (remove-process-from process kell)
    (add-process (sub-reduce process) kell)))

(defun add-process (process kell &optional watchp)
  (%add-process (expand-concretions process) kell watchp))

(defgeneric %add-process (process kell &optional watchp)
  (:documentation "Pushes PROCESS onto the given KELL.")
  (:method :before ((process agent) kell &optional watchp)
    "Sometimes a process that was matched gets re-added (as in `trigger*`), so
     we need to make sure that it is not seen as dead when re-added."
    (declare (ignore kell watchp))
    (setf (deadp process) nil))
  (:method ((process restriction-abstraction) (kell kell) &optional watchp)
    (add-process (sub-reduce process) kell watchp))
  (:method ((process agent) (kell kell) &optional watchp)
    (setf (state kell) (compose process (state kell)))
    (when watchp (watch process))
    (activate-process process kell))
  (:method ((process parallel-composition) (kell kell) &optional watchp)
    (kell-calculus::map-parallel-composition (alexandria:rcurry #'add-process kell watchp)
                                             process))
  (:method ((process kell) (kell kell) &optional watchp)
    (declare (ignorable watchp)) ; FIXME: not really, but CCL complains
    (if (gethash (name process) (kells kell))
        (error 'duplicate-kell-name-error :name (name process))
        (progn
          (call-next-method)
          (activate-process (state process) process)))))

(defun get-cpu-count ()
  #+ccl (ccl:cpu-count)
  #-(or ccl)
  (progn (princ "How many CPUs/cores are in your computer? ")
         (read)))

(defun start (&key cpu-count local-kell port-number)
  (unless cpu-count (setf cpu-count (get-cpu-count)))
  (let* ((use-network-p (or local-kell port-number)))
    (setf *current-pattern-language* +lax-fraktal+
          *top-kell* (make-instance 'kell
                                    :name (make-instance 'global-name
                                                         :label 'top))
          *local-kell* (when local-kell (intern local-kell)))
    (ccl::def-standard-initial-binding *current-pattern-language* +lax-fraktal+)
    (ccl::def-standard-initial-binding *package* (find-package :kilns-user))
    (ccl::def-standard-initial-binding *readtable* *kilns-readtable*)
    ;;(when use-network-p (start-kilns-listener port-number))
    (setf  (parent *top-kell*)
           (make-instance 'kell
                          :name (make-instance 'global-name :label 'outside)
                          :state *top-kell*))
    ;; dummy kell for now, to handle locking and other places we refer to
    ;; parents
    (setf (parent (parent *top-kell*))
          (make-instance (if use-network-p 'network-kell 'kell)
                         :name (make-instance 'global-name
                                              :label 'network)))
    (add-process (eval '(load "library/os/posix")) (parent *top-kell*))
    (add-process (eval '(par
                         (load "library/replication")
                         (kilns-user::trigger*
                          (up (message kilns-user::os
                               (process-variable kilns-user::message)))
                          kilns-user::message)))
                 *top-kell*))
  (start-kilns cpu-count))

(defun stop (kilns)
  (makunbound '*top-kell*)
  (mapc #'destroy-thread kilns)
  (clear-events))

(defun run-toplevel (&rest keys &key cpu-count local-kell port-number)
  (let ((kilns))
    (unwind-protect
         (progn
           (setf kilns (apply #'start keys))
           (toplevel))
      (stop kilns))))

(defgeneric toplevel (&optional top-kell))

(defmethod toplevel (&optional (top-kell *top-kell*))
  (let ((current-kell top-kell)
        (*package* (find-package :kilns-user))
        (*readtable* *kilns-readtable*))
    (defglobal move-up () nil
      (setf current-kell (parent current-kell))
      +null-process+)
    (defglobal move-down (kell-name) nil
      (let ((new-kell (car (gethash kell-name (kells current-kell)))))
        (if new-kell
            (setf current-kell new-kell)
            (error 'no-such-kell-error
                   :name kell-name :container current-kell)))
      +null-process+)
    (defglobal system-state () nil
      "Prints the current kell."
      ;; TODO: It would be great to pretty-print this, but just setting
      ;;       *PRINT-PRETTY* isn't enough.
      (print current-kell)
      +null-process+)
    (loop do
         (printk "~a> " (name current-kell))
         (handler-case (add-process (eval (read)) current-kell)
           (end-of-file () (return))
           (kiln-error (c) (handle-error c))))
    (remhash 'kilns-user::move-up *global-definitions*)
    (remhash 'kilns-user::move-down *global-definitions*)
    (remhash 'kilns-user::system-state *global-definitions*)))

(defgeneric remove-process (process)
  (:method ((process message))
    (let ((kell (parent process)))
      (remove-process-from process kell)
      (setf (gethash (name process) (messages kell))
            (delete process (gethash (name process) (messages kell))))))
  (:method ((process kell))
    (let ((kell (parent process)))
      (remove-events-for-kell process)
      (remove-process-from process kell)
      (setf (gethash (name process) (kells kell))
            (delete process (gethash (name process) (kells kell))))))
  (:method ((process pattern-abstraction))
    (let ((kell (parent process)))
      (remove-process-from process kell)
      (mapc (lambda (proc)
              (setf (gethash (name proc) (local-patterns kell))
                    (delete process
                            (gethash (name proc) (local-patterns kell)))))
            (local-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (down-patterns kell))
                    (delete process
                            (gethash (name proc) (down-patterns kell)))))
            (down-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (up-patterns kell))
                    (delete process (gethash (name proc) (up-patterns kell)))))
            (up-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (kell-patterns kell))
                    (delete process
                            (gethash (name proc) (kell-patterns kell)))))
            (kell-message-pattern (pattern process))))))

(defun trigger-process (trigger mapping watchp)
  "Activates process after substituting the process-variables in the trigger."
  (remove-process trigger)
  (add-process (substitute (process trigger) mapping)
               (parent trigger)
               watchp))

(defun activate-continuation (process)
  (remove-process process)
  (add-process (continuation process) (parent process) (watchp process)))

(defun execute-match (trigger processes substitutions)
  (let ((watchp (some #'watchp (cons trigger processes))))
    (when watchp
      (log-watched-reaction trigger processes))
    (setf (deadp trigger) t)
    (mapc (lambda (process) (setf (deadp process) t)) processes)
    (trigger-process trigger substitutions watchp))
  (mapc #'activate-continuation processes))

(defun find-kells-to-lock (trigger)
  "Returns a list of kells whose lock must be acquired (in this order) before we
   can attempt a match."
  (append (when (up-message-pattern (pattern trigger))
            (list (parent (parent trigger))))
          ;; always lock the current, because even if there isn't a local or
          ;; kell pattern, any result process would affect this kell
          (list (parent trigger))
          (when (down-message-pattern (pattern trigger))
            (subkells (parent trigger)))))

(defun select-matching-pattern (patterns live-process)
  (dolist (trigger patterns)
    (let ((locked-kells (find-kells-to-lock trigger)))
      (mapc (lambda (kell)
              (acquire-lock (lock kell) t)
              (setf (kell-calculus::activep kell) t))
            locked-kells)
      (unwind-protect
          (if (deadp live-process)
            (return nil)
            (when (not (deadp trigger))
              (multiple-value-bind (substitutions processes)
                  (match (pattern trigger) (parent trigger))
                (when processes
                  (execute-match trigger processes substitutions)
                  (return t)))))
        (mapc (lambda (kell)
                (setf (kell-calculus::activep kell) nil)
                (release-lock (lock kell)))
              locked-kells)))))

(defun find-triggers-matching-message (name kell)
  "Collect down-patterns from parent kell, up-patterns from subkells, and local-
   and kell-patterns from the given kell."
  (remove-duplicates (append (gethash name (local-patterns kell))
                             (handler-case
                                 (gethash name (down-patterns (parent kell)))
                               (unbound-slot () nil))
                             (mappend (lambda (subkell)
                                        (gethash name (up-patterns subkell)))
                                      (subkells kell)))))

(defgeneric match-on (process kell)
  (:documentation "Tries to find a match for all the patterns that could match
                   channel NAME in KELL.")
  (:method :around (process kell)
    ;; FIXME: kell is _not_ ignorable, but CCL complains
    (declare (ignorable kell))
    (when (not (deadp process))
      (handler-case (call-next-method)
        (kiln-error (c) (handle-error c)))))
  (:method ((process message) (kell kell))
    "Find all triggers that could match – up, down, or local."
    (select-matching-pattern (find-triggers-matching-message (name process)
                                                             kell)
                             process))
  (:method ((process kell) (kell kell))
    "Find all triggers that could match."
    (select-matching-pattern (gethash (name process) (kell-patterns kell))
                             process))
  (:method ((process pattern-abstraction) (kell kell))
    "Just match on the new trigger."
    (select-matching-pattern (list process) process)))

;;; TODO: These are definitions for the runtime functions for
;;;       abstractions and concretions. They should be integrated with
;;;       the ones above once we start actually using them.

(defmethod collect-channel-names ((process kell-abstraction) (kell kell))
  (let ((name (name process)))
    (push process (gethash name (kells kell)))
    (list (list process kell))))
(defmethod collect-channel-names ((process concretion) (kell kell))
  ;;; FIXME: should make sure (names process) is empty, or something
  (append (collect-channel-names (messages process) kell)
          (collect-channel-names (continuation process) kell)))

(defmethod activate-process ((process kell-abstraction) (kell kell))
  (if (gethash (name process) (kells kell))
    (error 'duplicate-kell-name-error :name (name process))
    (progn
      (call-next-method)
      (activate-process (abstraction process) process))))
(defmethod activate-process
           ((process application-abstraction) (kell kell))
  (remove-process process)
  (add-process (@ (abstraction process) (concretion process)) kell))
(defmethod activate-process ((process concretion) (kell kell))
  (remove-process process)
  (add-process (compose (sub-reduce (messages process))
                        (sub-reduce (continuation process)))
               kell))

(defmethod %add-process ((process kell-abstraction) (kell kell) &optional watchp)
  (declare (ignore watchp))
  (if (gethash (name process) (kells kell))
    (error 'duplicate-kell-name-error :name (name process))
    (progn
      (call-next-method)
      (activate-process (abstraction process) process))))
(defmethod %add-process ((process concretion) (kell kell) &optional watchp)
  (setf (state kell) (compose (sub-reduce process) (state kell)))
  (when watchp (watch process))
  (activate-process process kell))

;; FIXME: this can't currently work, ADD-PROCESS doesn't know _where_ in the
;;        process the concretion should be placed (although it's possible
;;        there's only one option for each process).
(defun apply-definition (definition concretion)
  ;; NOTE: We actually modify objects here, would be nice if we could do this functionally
  (let ((parent (parent concretion))
        (new-process (@ definition concretion)))
    (remove-process-from concretion parent)
    (typecase parent
      (message (setf (argument parent) (compose new-process (argument parent))))
      (kell (setf (state parent) (compose new-process (state parent))))
      (parallel-composition (compose new-process parent)) ; FIXME: won't work
      (trigger (setf (local-message-pattern (pattern parent))
                     (append (kell-calculus::messages-in new-process)
                             (local-message-pattern (pattern parent))))))))

(defgeneric expand-concretions (process)
  (:documentation
   "In addition to regular contexts and execution contexts, we also
    have matching contexts (noted M) that specify where concretions
    must be expanded to prevent blocking a match.

        M ::= · | M▹P | νa.M | (P|M) | a[M].P | a⟨M⟩.P

    Expands any accessible concretions in the given process and returns two
    values the expanded process (possibly the same as the argument if there
    were no concretions) and a boolean indicating whether the process was
    altered. This can't just be done in add-/activate-process because we also
    need to do this for inactive processes that could be involved in a match.")
  (:method-combination contract)
  (:method :guarantee "process hasn't changed if there was no expansion"
      (process)
    (multiple-value-bind (new-process contained-expansion-p) (results)
      (not (eq contained-expansion-p (eq process new-process)))))
  (:method ((process named-concretion))
    (let ((definition (gethash (name process) *global-definitions*)))
      (if definition
          (let ((new-process (@ definition process)))
            (if (eq new-process process)
                (values process nil)
                (values (expand-concretions new-process) t)))
          (progn
            (push process
                  (gethash (name process) *delayed-concretions*))
            (values process nil)))))
  (:method (process)
    ;; FIXME: won't need this once we actually read everything as a real process
    (values process nil))
  (:method ((process null-process))
    (values process nil))
  (:method ((process message))
    (multiple-value-bind (new-argument contained-expansion-p)
        (expand-concretions (argument process))
      (values (if contained-expansion-p
                  (make-instance 'message
                                 :name (name process)
                                 :argument new-argument
                                 :continuation (continuation process))
                  process)
              contained-expansion-p)))
  (:method ((process parallel-composition))
    (let* ((contained-expansion-p nil)
           (new-processes
            (map-parallel-composition (lambda (sub-process)
                                        (multiple-value-bind (new-process
                                                              expandedp)
                                            (expand-concretions sub-process)
                                          (when expandedp
                                            (setf contained-expansion-p t))
                                          new-process))
                                      process)))
      (values (if contained-expansion-p
                  (apply #'parallel-composition new-processes)
                  process)
              contained-expansion-p)))
  (:method ((process kell))
    (multiple-value-bind (new-state contained-expansion-p)
        (expand-concretions (state process))
      (values (if contained-expansion-p
                  (make-instance 'kell
                                 :name (name process)
                                 :state new-state
                                 :continuation (continuation process))
                  process)
              contained-expansion-p)))
  (:method ((process trigger))
    ;; TODO: Don't expand anything here – the pattern gets expanded when the
    ;;       trigger is activated, and the process is expanded when the trigger
    ;;       matches.
    (multiple-value-bind (new-pattern contained-expansion-p)
        (expand-concretions (pattern process))
      (values (if contained-expansion-p
                  (make-instance 'trigger
                                 :pattern new-pattern
                                 :process (process process))
                  process)
              contained-expansion-p)))
  (:method ((process pattern))
    (let ((contained-expansion-p nil))
      (flet ((expand-subpattern (subpattern)
               (multiple-value-bind (new-pattern expandedp)
                   (expand-concretions subpattern)
                 (when expandedp (setf contained-expansion-p t))
                 new-pattern)))
        (let ((new-patterns (mapcar #'expand-subpattern
                                    (named-concretions process)))
              (new-locals (mapcar #'expand-subpattern
                                  (local-message-pattern process)))
              (new-ups (mapcar #'expand-subpattern
                               (up-message-pattern process)))
              (new-downs (mapcar #'expand-subpattern
                                 (down-message-pattern process)))
              (new-kells (mapcar #'expand-subpattern
                                 (kell-message-pattern process))))
          (let ((new-kell-pattern (remove-if-not (alexandria:rcurry #'typep
                                                                    'kell)
                                                 new-patterns))
                (new-messages (remove-if-not (alexandria:rcurry #'typep
                                                                'message)
                                             new-patterns))
                (new-concretions (remove-if-not (alexandria:rcurry #'typep
                                                                   'named-concretion)
                                                new-patterns)))
            (let ((new-local-pattern (remove-if-not (alexandria:rcurry #'typep 'null-process)
                                                    new-messages
                                                    :key #'continuation))
                  (new-up-pattern (remove-if-not (alexandria:curry #'eq 'up)
                                                    new-messages
                                                    :key #'continuation))
                  (new-down-pattern (remove-if-not (alexandria:curry #'eq 'down)
                                                    new-messages
                                                    :key #'continuation)))
              (values (if contained-expansion-p
                          (make-instance 'pattern
                                         :local-message-pattern (append new-locals
                                                                        new-local-pattern)
                                         :up-message-pattern (append new-ups new-up-pattern)
                                         :down-message-pattern (append new-downs new-down-pattern)
                                         :kell-message-pattern (append new-kells new-kell-pattern)
                                         :named-concretions new-concretions
                                         :placeholders (placeholders process))
                          process)
                      contained-expansion-p)))))))
  (:method ((process restriction))
    (values (expand-concretions (sub-reduce process))
            t))
  (:method ((process definition))
    (values process nil)))

(defmethod %add-process ((process definition) (kell kell) &optional watchp)
  (setf (gethash (name process) *global-definitions*) process)
  (activate-process process kell))

(defmethod activate-process ((process definition) (kell kell))
  (mapc (alexandria:curry #'apply-definition process)
        (gethash (name process) *delayed-concretions*)))

(defmethod %add-process ((process named-concretion) (kell kell) &optional watchp)
  (setf (parent process) kell
        (state kell) (compose process (state kell)))
  (when watchp (watch process))
  (activate-process process kell))

(defmethod activate-process ((process named-concretion) (kell kell))
  (multiple-value-bind (expanded-process contained-expansion-p)
      (expand-concretions process)
    (when contained-expansion-p
      (remove-process-from process kell)
      (add-process expanded-process kell))))

;;; Initialization

(defglobal lisp (&rest processes) t
  "Call back into the lisp from Kilns. Provides a SUBSTITUTE-VARIABLES macro
   that allows us to pass lisp variables back to nested processes."
  (cl:eval `(macrolet ((substitute-variables ((&rest variables) &body processes)
                         `(let ((env (make-empty-environment)))
                            ,@(mapcar (lambda (var)
                                        `(unify (intern (format nil "?~a"
                                                                ',var))
                                                ,var
                                                env))
                                      variables)
                            (substitute (parallel-composition ,@processes)
                                        env))))
              ,@processes)))

;;; FIXME: for networking, load needs to be able to take a path to a subkell
;;;        that represents what is to be run in the local instance. It should
;;;        work as if the following (illegal) trigger were used:
;;;            (trigger [path [to [correct [kell ?process]]]] ?process)
(defglobal load (file-name
                 &key
                 (verbose *load-verbose*)
                 (print *load-print*)
                 (if-does-not-exist :error)) nil
  (declare (ignore if-does-not-exist verbose))
  (let ((full-name (handler-case
                       (truename (asdf:system-relative-pathname :kilns file-name
                                                                :type "kiln"))
                     (file-error ()
                       (merge-pathnames file-name
                                        (make-pathname :type "kiln"))))))
    (let ((processes (with-open-file (stream full-name
                                                 :external-format :utf-8)
                           (loop for value = (read stream nil)
                              while value
                              do (if print (print value) value)
                              collecting value))))
      (eval `(par ,@processes)))))

(defglobal list (&rest processes) nil
  (let ((index 0))
    (apply #'parallel-composition
           (mapcar (lambda (process)
                     (make-instance 'message
                                    :name (find-or-add-global-name (incf index))
                                    :argument process))
                   processes))))
