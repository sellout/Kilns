;;;(defpackage kiln-runner
;;;  (:use #:cl #:kilns)
;;;  (:shadow #:load #:read #:eval)
;;;  (:export #:load #:read #:eval))

(in-package #:kilns)

(defvar *top-kell*)

(let ((lock (make-lock "print-lock")))
  (defun printk (&rest arguments)
    "This is our log-to-screen function. Works like format, but makes sure messages are
     printed atomically. It also starts each new message on its own line (since there's
     no guarantee that the previous message is even from the same thread, this is
     totally reasonable)."
    (with-lock-held (lock)
      (apply #'format t "~&~@?" arguments))))

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
      (setf *event-queue* '()))))

(defmacro lock-neighboring-kells ((kell) &body body)
  "This ensures that we always lock kells from the outermost to the innermost,
   preventing deadlocks"
  (let ((kellvar (gensym "KELL")))
    `(let ((,kellvar ,kell))
       (with-lock-held ((lock (parent ,kellvar)))
         (with-lock-held ((lock ,kellvar))
           ;;; FIXME: need to lock all immediate children
           ,@body)))))

(defun run-kiln ()
  (loop do (handler-case (let ((event (pop-event)))
                           (if event
                             (apply (car event) (cdr event))
                             (with-lock-held (*dummy-wait-lock*)
                               (condition-wait *new-events* *dummy-wait-lock*))))
             (error (c) (printk "ERROR: ~a~%" c)))))

(defun start-kilns (count)
  (loop for i from 1 to count
    collecting (make-thread #'run-kiln :name (format nil "kiln ~d" i))))

(defgeneric apply-restriction (local-name global-name process)
  (:method (local-name global-name process)
    (declare (ignore local-name global-name))
    process)
  (:method (local-name global-name (process message))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (apply-restriction local-name global-name (process process))
    (apply-restriction local-name global-name (continuation process))
    process)
  (:method (local-name global-name (process kell))
    (if (eql (name process) local-name)
      (setf (name process) global-name))
    (apply-restriction local-name global-name (process process))
    (apply-restriction local-name global-name (continuation process))
    process)
  (:method (local-name global-name (process parallel-composition))
    (mapc (lambda (message)
            (apply-restriction local-name global-name message))
          (messages process))
    (mapc (lambda (kell)
            (apply-restriction local-name global-name kell))
          (kells process))
    (mapc (lambda (trigger)
            (apply-restriction local-name global-name trigger))
          (triggers process))
    process)
  (:method (local-name global-name (process pattern))
    (mapc (lambda (message)
            (apply-restriction local-name global-name message))
          (local-message-pattern process))
    (mapc (lambda (message)
            (apply-restriction local-name global-name message))
          (down-message-pattern process))
    (mapc (lambda (message)
            (apply-restriction local-name global-name message))
          (up-message-pattern process))
    (mapc (lambda (message)
            (apply-restriction local-name global-name message))
          (kell-message-pattern process))
    process)
  (:method (local-name global-name (process restriction))
    (apply-restriction local-name global-name (process process))
    process)
  (:method (local-name global-name (process trigger))
    (apply-restriction local-name global-name (pattern process))
    (apply-restriction local-name global-name (process process))
    process))


(defgeneric activate-process (process kell)
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
  (:method ((process restriction) (kell kell))
    (let ((global-name (gensym (format nil "~a" (name process)))))
      (activate-process (apply-restriction (name process)
                                           global-name
                                           (process process))
                        kell))))

(defgeneric add-process (process kell)
  (:method (process kell)
    ;; lets us run normal functions without consequence
    (declare (ignore process kell))
    (values))
  (:method ((process restriction) (kell kell))
    (let ((global-name (gensym (format nil "~a" (name process)))))
      (add-process (apply-restriction (name process) global-name (process process))
                   kell)))
  (:method ((process process) (kell kell))
    (setf (process kell) (compose-processes process (process kell)))
    (activate-process process kell))
  (:method ((process parallel-composition) (kell kell))
    (map-parallel-composition (lambda (sub-process)
                                (add-process sub-process kell))
                              process))
  (:method ((process kell) (kell kell))
    (call-next-method)
    (activate-process (process process) process)))

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
  (:method ((process trigger) (kell kell))
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
  (:method ((process (eql null-process)) (kell kell))
    (declare (ignore kell))
    '()))

(defun get-cpu-count ()
  (princ "How many CPUs/cores are in your computer? ")
  (read))

(defun toplevel (&optional cpu-count)
  (unless cpu-count (setf cpu-count (get-cpu-count)))
  (let* ((*top-kell* (make-instance 'kell :name (gensym "LOCALHOST")))
         (kilns (start-kilns cpu-count))
         (*package* (find-package :kilns-user))
         (*readtable* *kilns-readtable*))
    ;; dummy kell for now, to handle locking and other places we refer to parents
    (setf (parent *top-kell*) (make-instance 'kell
                                :name (gensym "NETWORK") :process *top-kell*))
    (unwind-protect
        (loop do
          (printk "> ")
          (handler-case (let ((process (eval (read))))
                          (printk "~a~%" process)
                          (add-process process *top-kell*))
            (end-of-file () (return))
            (error (c) (printk "ERROR: ~a~%" c))))
      (mapc #'destroy-thread kilns)
      (clear-events))))

(defgeneric duplicate-process (process)
  (:method (process)
    process)
  (:method ((process message))
    (make-instance 'message
      :name (duplicate-process (name process))
      :process (duplicate-process (process process))
      :continuation (duplicate-process (continuation process))))
  (:method ((process kell))
    (make-instance 'kell
      :name (duplicate-process (name process))
      :process (duplicate-process (process process))
      :continuation (duplicate-process (continuation process))))
  (:method ((process trigger))
    (make-instance 'trigger
      :pattern (duplicate-process (pattern process))
      :process (duplicate-process (process process))))
  (:method ((process parallel-composition))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (messages pc) (mapcar #'duplicate-process (messages process))
             (kells pc) (mapcar #'duplicate-process (kells process))
             (triggers pc) (mapcar #'duplicate-process (triggers process))
             (process-variables pc) (mapcar #'duplicate-process
                                            (process-variables process)))
      pc))
  (:method ((process restriction))
    (make-instance 'restriction
      :name (duplicate-process (name process))
      :process (duplicate-process (process process)))))

(defgeneric remove-process (process)
  (:method ((process message))
    (let ((kell (parent process)))
      (remove-process-from process kell)
      (setf (gethash (name process) (messages kell))
            (delete process (gethash (name process) (messages kell))))))
  (:method ((process kell))
    ;; FIXME: also need to remove anything referring to this kell from the event-queue
    (let ((kell (parent process)))
      (remove-process-from process kell)
      (setf (gethash (name process) (kells kell))
            (delete process (gethash (name process) (kells kell))))))
  (:method ((process trigger))
    (let ((kell (parent process)))
      (remove-process-from process kell)
      (mapc (lambda (proc)
              (setf (gethash (name proc) (local-patterns kell))
                    (delete process (gethash (name proc) (local-patterns kell)))))
            (local-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (down-patterns kell))
                    (delete process (gethash (name proc) (down-patterns kell)))))
            (down-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (up-patterns kell))
                    (delete process (gethash (name proc) (up-patterns kell)))))
            (up-message-pattern (pattern process)))
      (mapc (lambda (proc)
              (setf (gethash (name proc) (kell-patterns kell))
                    (delete process (gethash (name proc) (kell-patterns kell)))))
            (kell-message-pattern (pattern process))))))

(defun replace-variables (process mapping &optional ignored-vars)
  (let ((substituted-processes
         (mapcar (lambda (process-variable)
                   (when (not (find (name process-variable) ignored-vars
                                    :key #'name))
                     (printk "Replacing ~a with ~a in ~a~%"
                             process-variable
                             (find-process-variable-value process-variable mapping)
                             process)
                     (setf process (remove-process-from process-variable process))
                     (find-process-variable-value process-variable mapping)))
                 (process-variables-in process))))
    (reduce #'compose-processes (cons process substituted-processes))))

(defgeneric replace-name (name mapping &optional ignored-vars)
  (:method ((name symbol) mapping &optional ignored-vars)
    (declare (ignore mapping ignored-vars))
    name)
  (:method ((name name-variable) mapping &optional ignored-vars)
    (if (find (name name) ignored-vars :key #'name)
      name
      (find-name-variable-value name mapping))))

;;; FIXME: this is crying out for some simplification
(defgeneric substitute-variables (mapping process &optional ignored-vars)
  (:method (mapping (process message) &optional ignored-vars)
    (mapc (lambda (proc) (substitute-variables mapping proc ignored-vars))
          (append (messages-in (process process))
                  (kells-in (process process))
                  (triggers-in (process process))
                  (messages-in (continuation process))
                  (kells-in (continuation process))
                  (triggers-in (continuation process))))
    (psetf (name process) (replace-name (name process) mapping ignored-vars)
           (process process) (replace-variables (process process) mapping ignored-vars)
           (continuation process) (replace-variables (continuation process) mapping
                                                     ignored-vars))
    (printk "new process: ~a~%" process))
  (:method (mapping (process kell) &optional ignored-vars)
    (mapc (lambda (proc) (substitute-variables mapping proc ignored-vars))
          (append (messages-in (process process))
                  (kells-in (process process))
                  (triggers-in (process process))
                  (messages-in (continuation process))
                  (kells-in (continuation process))
                  (triggers-in (continuation process))))
    (psetf (process process) (replace-variables (process process) mapping ignored-vars)
           (continuation process) (replace-variables (continuation process) mapping
                                                     ignored-vars))
    (printk "new process: ~a~%" process))
  (:method (mapping (process trigger) &optional ignored-vars)
    (setf ignored-vars (append (bound-names (pattern process))
                               (bound-variables (pattern process))
                               ignored-vars))
    (warn "ignoring ~a" ignored-vars)
    (mapc (lambda (proc) (substitute-variables mapping proc ignored-vars))
          (append (messages-in (process process))
                  (kells-in (process process))
                  (triggers-in (process process))))
    (psetf (process process) (replace-variables (process process) mapping
                                                ignored-vars))
    (printk "new process: ~a~%" process)))

(defmethod trigger-process ((trigger trigger) mapping)
  "Activates process after substituting the process-variables in the trigger."
  ;; FIXME: basically a copy of substitute-variables (t trigger), except we don't
  ;;        ignore the vars, because this is the trigger that's actually triggering.
  (let ((process (duplicate-process (process trigger))))
    (remove-process trigger)
    (mapc (lambda (proc) (substitute-variables mapping proc))
          (append (messages-in process)
                  (kells-in process)
                  (triggers-in process)))
    (let ((substituted-processes
           (mapcar (lambda (process-variable)
                     (printk "Replacing (top) ~a with ~a in ~a~%"
                             process-variable
                             (find-process-variable-value process-variable mapping)
                             process)
                     (setf process (remove-process-from process-variable process))
                     (find-process-variable-value process-variable mapping))
                   (process-variables-in (process trigger)))))
      (add-process (reduce #'compose-processes
                         (cons process
                               substituted-processes))
                   (parent trigger)))))

(defmethod activate-continuation (process)
    (remove-process process)
    (add-process (continuation process) (parent process)))

(defgeneric really-match-on (process kell)
  (:documentation "Tries to find a match for all the patterns that could match
                   channel NAME in KELL.")
  (:method ((process message) (kell kell))
    "Find all triggers that could match – up, down, or local."
    (let ((name (name process)))
      (catch 'match
        (mapc (lambda (trigger)
                (printk "attempt to match ~a against ~a~%" (pattern trigger) kell)
                (handler-case
                    (destructuring-bind (processes substitutions)
                                        (match (pattern trigger) (parent trigger))
                      (throw 'match (list trigger processes substitutions)))
                  (error () nil))) ; FIXME: this should be tighter
              (remove-duplicates (append (gethash name (local-patterns kell))
                                         (gethash name (down-patterns (parent kell)))
                                         (mapcan (lambda (subkell)
                                                   (gethash name
                                                            (up-patterns subkell)))
                                                 (subkells kell))))))))
  (:method ((process kell) (kell kell))
    "Find all triggers that could match."
    (catch 'match
      (mapc (lambda (trigger)
              (printk "attempt to match ~a against ~a~%" (pattern trigger) kell)
              (handler-case
                  (destructuring-bind (processes substitutions)
                                      (match (pattern trigger) kell)
                    (throw 'match (list trigger processes substitutions)))
                (error () nil)))
            (gethash (name process) (kell-patterns kell)))))
  (:method ((process trigger) (kell kell))
    "Just match on the new trigger."
    (printk "attempt to match ~a against ~a~%" (pattern process) kell)
    (handler-case
        (destructuring-bind (processes substitutions)
                            (match (pattern process) (parent process))
          (list process processes substitutions))
      (error () nil))))

(defun match-on (process kell)
  (printk "Trying to match ~a in ~a.~%" process kell)
  (lock-neighboring-kells (kell)
    (destructuring-bind (&optional trigger matched-processes substitutions)
                        (really-match-on process kell)
      (when (and trigger matched-processes)
        (printk "The pattern ~a will match the process ~a and result in the ~
                 process ~a.~%"
                (pattern trigger) matched-processes (process trigger))
        (trigger-process trigger substitutions)
        (mapc #'activate-continuation matched-processes)
        (printk "~a~%" (parent kell))))))

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
