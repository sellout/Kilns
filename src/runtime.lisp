;;;(defpackage kiln-runner
;;;  (:use #:cl #:kilns)
;;;  (:shadow #:load #:read #:eval)
;;;  (:export #:load #:read #:eval))

(in-package #:kilns)

(defvar *top-kell*)
(defvar *local-kell*)

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
      (setf *event-queue* (delete kell *event-queue* :key #'third)))))

(defmacro lock-neighboring-kells ((kell) &body body)
  "This ensures that we always lock kells from the outermost to the innermost,
   preventing deadlocks"
  (let ((kellvar (gensym "KELL")))
    `(let ((,kellvar ,kell))
       (with-lock-held ((lock (parent ,kellvar)))
         (with-lock-held ((lock ,kellvar))
           ;; FIXME: this should probably use some fancy WITH-LOCK-HELD macroexpansion
           ;;        in order to UNWIND-PROTECT all of the lock releases 
           (let ((subkells (subkells ,kellvar)))
             (mapcar (lambda (subkell)
                       (acquire-lock (lock subkell) t))
                     subkells)
             ,@body
             (mapcar (lambda (subkell)
                       (release-lock (lock subkell)))
                     (reverse subkells))))))))

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
                             (apply (car event) (cdr event))
                             (with-lock-held (*dummy-wait-lock*)
                               (condition-wait *new-events*
                                               *dummy-wait-lock*))))
             (kiln-error (c) (handle-error c)))))

(defun start-kilns (count)
  (loop for i from 1 to count
    collecting (make-thread #'run-kiln :name (format nil "kiln ~d" i))))

(defgeneric expand-restriction (restriction)
  (:method ((restriction restriction-abstraction))
    (let ((abstraction (abstraction restriction)))
      (mapc (lambda (name)
              (setf abstraction
                    ;; TODO: apply-restriction should handle all names
                    ;;       at once, rather than one at a time
                    (apply-restriction name
                                       (gensym (format nil "~a-" name))
                                       abstraction)))
            (names restriction))
      abstraction))
  (:method ((restriction concretion))
    (if (length (restricted-names restriction))
      (let ((messages (messages restriction))
            (continuation (continuation restriction)))
        (mapc (lambda (name)
                (psetf messages
                       (apply-restriction name
                                          (gensym (format nil "~a-"
                                                          name))
                                          messages)
                       continuation
                       (apply-restriction name
                                          (gensym (format nil "~a-" name))
                                          continuation)))
              (restricted-names restriction))
        (make-instance (class-of restriction)
          :messages messages :continuation continuation))
      restriction)))

(defgeneric apply-restriction (local-name global-name process &optional expandp)
  (:documentation "DESTRUCTIVE. Returns the process with all restrictions
                   expanded to have unique names.")
  (:method (local-name global-name process &optional (expandp t))
    (declare (ignore local-name global-name expandp))
    process)
  (:method (local-name global-name (process message) &optional (expandp t))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (psetf (process process)
           (apply-restriction local-name global-name (process process) expandp)
           (continuation process)
           (apply-restriction local-name global-name (continuation process) expandp))
    process)
  (:method (local-name global-name (process kell) &optional (expandp t))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (psetf (process process)
           (apply-restriction local-name global-name (process process) expandp)
           (continuation process)
           (apply-restriction local-name global-name (continuation process) expandp))
    process)
  (:method (local-name global-name (process parallel-composition) &optional (expandp t))
    (apply #'parallel-composition
           (map-parallel-composition (lambda (proc)
                                       (apply-restriction local-name global-name proc
                                                          expandp))
                                     process)))
  (:method (local-name global-name (process pattern) &optional (expandp t))
    (psetf (local-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (local-message-pattern process))
           (down-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (down-message-pattern process))
           (up-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (up-message-pattern process))
           (kell-message-pattern process)
           (mapcar (lambda (message)
                     (apply-restriction local-name global-name message expandp))
                   (kell-message-pattern process)))
    process)
  (:method (local-name global-name (process restriction-abstraction)
            &optional (expandp t))
    (if expandp
      (apply-restriction local-name global-name (expand-restriction process)
                         expandp)
      (if (find local-name (names process))
        process
        (make-instance (class-of process)
          :names (names process)
          :abstraction (apply-restriction local-name
                                          global-name
                                          (abstraction process)
                                          expandp)))))
  (:method (local-name global-name (process pattern-abstraction)
            &optional (expandp t))
    (make-instance (class-of process)
      :pattern (apply-restriction local-name global-name (pattern process)
                                  expandp)
      :process (apply-restriction local-name global-name (process process)
                                  nil))))

(defgeneric collect-channel-names (process kell)
  (:documentation "This returns a list of events to add to the event queue.")
  (:method ((process process-variable) (kell kell))
    ;; FIXME: not good enough. Need to prevent it from getting into the kell.
    (break "Can't have a free variable (~a) in an active kell (~a)." process kell))
  (:method ((process parallel-composition) (kell kell))
    ;; FIXME: I don't think this ever gets called
    (apply #'append
           (map-parallel-composition (lambda (proc)
                                       (collect-channel-names proc kell))
                                     process)))
  (:method ((process message) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (messages kell)))
      (list (list #'match-on process kell))))
  (:method ((process kell) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (kells kell)))
      (list (list #'match-on process kell))))
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
    (list (list #'match-on process kell)))
  (:method ((process null-process) (kell kell))
    (declare (ignore kell))
    '()))

(defgeneric activate-process (process kell)
  (:method ((process cons) (kell kell))
    (remove-process-from process kell)
    (add-process (eval process) kell))
  (:method ((process process) (kell kell))
    (setf (parent process) kell)
    (mapc #'push-event (collect-channel-names process kell)))
  (:method ((process kell) (kell kell))
    (call-next-method)
    (activate-process (process process) process))
  (:method ((process parallel-composition) (kell kell))
    (map-parallel-composition (lambda (sub-process)
                                (activate-process sub-process kell))
                              process))
  (:method ((process pattern-abstraction) (kell kell))
    (setf (parent process) kell)
    (mapc #'push-event (collect-channel-names process kell)))
  (:method ((process restriction-abstraction) (kell kell))
    (remove-process process)
    (add-process (expand-restriction process) kell)))

(defgeneric add-process (process kell)
  (:documentation "Pushes PROCESS onto the given KELL.")
  (:method (process kell)
    "Handles any other “primitive” processes (strings, numbers, etc.)"
    (declare (ignore process kell))
    (values))
  (:method ((process cons) (kell kell))
    (add-process (eval process) kell))
  (:method ((process restriction-abstraction) (kell kell))
    (add-process (expand-restriction process) kell))
  (:method ((process agent) (kell kell))
    (setf (process kell) (compose-processes process (process kell)))
    (activate-process process kell))
  (:method ((process parallel-composition) (kell kell))
    (map-parallel-composition (lambda (sub-process)
                                (add-process sub-process kell))
                              process))
  (:method ((process kell) (kell kell))
    (if (gethash (name process) (kells kell))
        (error 'duplicate-kell-name-error :name (name process))
        (progn
          (call-next-method)
          (activate-process (process process) process)))))

(defun get-cpu-count ()
  (princ "How many CPUs/cores are in your computer? ")
  (read))

(let ((current-kell))
  (defun move-up ()
    (setf current-kell (parent current-kell))
    (values))
  (defun move-down (kell-name)
    (let ((new-kell (car (gethash kell-name (kells current-kell)))))
      (if new-kell
        (setf current-kell new-kell)
        (error 'no-such-kell-error :name kell-name :container current-kell)))
    (values))
  (defun toplevel (&optional cpu-count local-kell)
    (unless cpu-count (setf cpu-count (get-cpu-count)))
    (let* ((*top-kell* (make-instance 'kell :name (gensym "TOP")))
           (*package* (find-package :kilns-user))
           (*readtable* *kilns-readtable*)
           (*local-kell* (when local-kell (intern local-kell))))
      (ccl::def-standard-initial-binding *package* (find-package :kilns-user))
      (ccl::def-standard-initial-binding *readtable* *kilns-readtable*)
      (let ((kilns (start-kilns cpu-count)))
        ;; dummy kell for now, to handle locking and other places we refer to
        ;; parents
        (setf current-kell *top-kell*
              (parent *top-kell*)
              (make-instance 'kell :name (gensym "OUTSIDE") :process *top-kell*))
        (unwind-protect
            (loop do
              (printk "~a> " (name current-kell))
              (handler-case (let ((process (eval (read))))
                              (add-process process current-kell))
                (end-of-file () (return))
                (kiln-error (c) (handle-error c))))
          (mapc #'destroy-thread kilns)
          (clear-events))))))

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

(defgeneric replace-name (name mapping &optional ignored-vars)
  (:method (name mapping &optional ignored-vars)
    (declare (ignore mapping ignored-vars))
    name)
  (:method ((name symbol) mapping &optional ignored-vars)
    (if (find name ignored-vars)
      name
      (or (find-symbol-value name mapping)
          name)))
  (:method ((name process-variable) mapping &optional ignored-vars)
    ;; FIXME: this method only exists because sometimes '?x' is being read as a
    ;;        process-variable instead of read as a name-variable
    (if (find (name name) ignored-vars)
      name
      (find-process-variable-value name mapping)))
  (:method ((name name-variable) mapping &optional ignored-vars)
    (if (find (name name) ignored-vars)
      name
      (find-name-variable-value name mapping))))

(defgeneric replace-variables (process mapping &optional ignored-vars)
  (:method :around ((process process) mapping &optional ignored-vars)
    "Make sure conses are evaled as soon as there are no free vars, but no sooner."
    (declare (ignore mapping ignored-vars))
    (let ((new-process (call-next-method)))
      (map-process (lambda (proc)
                     (if (and (listp proc) (not (free-variables proc)))
                       (eval proc)
                       proc))
                   new-process)))
  (:method (process mapping &optional ignored-vars)
    "This just skips over primitives."
    (declare (ignore mapping ignored-vars))
    process)
  (:method ((name symbol) mapping &optional ignored-vars)
    (if (find name ignored-vars)
      name
      (or (find-symbol-value name mapping)
          name)))
  (:method ((process cons) mapping &optional ignored-vars)
    (mapcar (lambda (item) (replace-variables item mapping ignored-vars))
            process))
  (:method ((process process-variable) mapping &optional ignored-vars)
    (if (find (name process) ignored-vars)
      process
      (find-process-variable-value process mapping)))
  (:method ((process message-structure) mapping &optional ignored-vars)
    (make-instance (class-of process)
      :name (replace-name (name process) mapping ignored-vars)
      :process (replace-variables (process process) mapping ignored-vars)
      :continuation (replace-variables (continuation process) mapping ignored-vars)))
  (:method ((process parallel-composition) mapping &optional ignored-vars)
    (map-process (lambda (proc) (replace-variables proc mapping ignored-vars))
                 process))
  (:method ((process pattern-abstraction) mapping &optional ignored-vars)
    (let ((ignored-vars (append (mapcar #'name (bound-names (pattern process)))
                                (mapcar #'name
                                        (bound-variables (pattern process)))
                                ignored-vars)))
      (make-instance (class-of process)
        :pattern (make-instance 'pattern
                   :local-message-pattern
                   (replace-variables (local-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :down-message-pattern
                   (replace-variables (down-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :up-message-pattern
                   (replace-variables (up-message-pattern (pattern process))
                                      mapping ignored-vars)
                   :kell-message-pattern
                   (replace-variables (kell-message-pattern (pattern process))
                                      mapping ignored-vars))
        :process (replace-variables (process process) mapping ignored-vars))))
  (:method ((process restriction-abstraction) mapping &optional ignored-vars)
    ;;; FIXME: these names should only be added to ignored NAME vars, but we
    ;;;        currently don't distinguish
    (let ((ignored-vars (append (names process) ignored-vars)))
      (make-instance (class-of process)
        :names (names process)
        :abstraction (replace-variables (abstraction process) mapping ignored-vars)))))

(defmethod trigger-process ((trigger pattern-abstraction) mapping)
  "Activates process after substituting the process-variables in the trigger."
  (remove-process trigger)
  (add-process (replace-variables (process trigger) mapping)
               (parent trigger)))

(defmethod activate-continuation (process)
    (remove-process process)
    (add-process (continuation process) (parent process)))

(defun select-matching-pattern (patterns)
  (loop for trigger in patterns
     for (processes substitutions)
       = (handler-case (match (pattern trigger) (parent trigger))
           (unification-failure () (list nil nil)))
     if processes
     return (progn
              (setf (deadp trigger) t)
              (mapc (lambda (process) (setf (deadp process) t)) processes)
              (list trigger processes substitutions))))

(defgeneric really-match-on (process kell)
  (:documentation "Tries to find a match for all the patterns that could match
                   channel NAME in KELL.")
  (:method ((process message) (kell kell))
    "Find all triggers that could match – up, down, or local."
    (let ((name (name process)))
      (select-matching-pattern
       (remove-duplicates (append (gethash name (local-patterns kell))
                                  (gethash name (down-patterns (parent kell)))
                                  (mappend (lambda (subkell)
                                             (gethash name
                                                      (up-patterns subkell)))
                                           (subkells kell)))))))
  (:method ((process kell) (kell kell))
    "Find all triggers that could match."
    (select-matching-pattern (gethash (name process) (kell-patterns kell))))
  (:method ((process pattern-abstraction) (kell kell))
    "Just match on the new trigger."
    (select-matching-pattern (list process))))

(defun match-on (process kell)
  (when (not (deadp process))
    (handler-case
        (lock-neighboring-kells (kell)
          (destructuring-bind (&optional trigger matched-processes substitutions)
              (really-match-on process kell)
            (when (and trigger matched-processes)
              (trigger-process trigger substitutions)
              (mapc #'activate-continuation matched-processes))))
      (kiln-error (c) (handle-error c)))))

(defun find-triggers-matching-message (name kell)
  "Collect down-patterns from parent kell, up-patterns from subkells, and local-
   and kell-patterns from the given kell."
  (append (mapcar (lambda (pattern)
                    (list pattern (parent kell)))
                  (gethash name (down-patterns (parent kell))))
          (mapcan (lambda (subkell)
                    (mapcar (lambda (pattern)
                              (list pattern subkell))
                            (gethash name (up-patterns subkell))))
                  (subkells kell))
          (mapcar (lambda (pattern)
                    (list pattern kell))
                  (gethash name (local-patterns kell)))))

;;; TODO: These are definitions for the runtime functions for
;;;       abstractions and concretions. They should be integrated with
;;;       the ones above once we start actually using them.

(defmethod apply-restriction
           (local-name global-name (process kell-abstraction)
            &optional (expandp t))
  (make-instance (class-of process)
    :name (if (eql (name process) local-name)
            global-name
            (name process))
    :abstraction (apply-restriction local-name
                                    global-name
                                    (abstraction process)
                                    expandp)
    :continuation (apply-restriction local-name
                                     global-name
                                     (continuation process)
                                     expandp)))
(defmethod apply-restriction
           (local-name global-name (process application-abstraction)
            &optional (expandp t))
  (make-instance (class-of process)
    :abstraction (apply-restriction local-name
                                    global-name
                                    (abstraction process)
                                    expandp)
    :concretion (apply-restriction local-name
                                   global-name
                                   (concretion process)
                                   expandp)))
(defmethod apply-restriction
           (local-name global-name (process concretion)
            &optional (expandp t))
  (if expandp
    (apply-restriction local-name
                       global-name
                       (expand-restriction process)
                       expandp)
    (if (find local-name (restricted-names process))
      process
      (make-instance (class-of process)
        :restricted-names (restricted-names process)
        :messages (apply-restriction local-name
                                     global-name
                                     (messages process)
                                     expandp)
        :continuation (apply-restriction local-name
                                      global-name
                                      (continuation process)
                                      expandp)))))

(defmethod collect-channel-names
           ((process kell-abstraction) (kell kell))
  (let ((name (name process)))
    (push process (gethash name (kells kell)))
    (list (list #'match-on process kell))))
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
(defmethod activate-process
           ((process concretion) (kell kell))
  (remove-process process)
  (add-process (compose (expand-restriction (messages process))
                        (expand-restriction (continuation process)))
               kell))

(defmethod add-process ((process kell-abstraction) (kell kell))
  (if (gethash (name process) (kells kell))
    (error 'duplicate-kell-name-error :name (name process))
    (progn
      (call-next-method)
      (activate-process (abstraction process) process))))
(defmethod add-process ((process concretion) (kell kell))
  (add-process (expand-restriction process) kell))
