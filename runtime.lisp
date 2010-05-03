;;;(defpackage kiln-runner
;;;  (:use #:cl #:kilns)
;;;  (:shadow #:load #:read #:eval)
;;;  (:export #:load #:read #:eval))

(in-package #:kilns)

(defvar *kilns-readtable* (copy-readtable))
(setf (readtable-case *kilns-readtable*) :invert)

(defvar *top-kell*)

;;; FIXME: make sure these threadsafe functions truly are

(defvar *new-events* (make-condition-variable))
(defvar *dummy-wait-lock* (make-lock "dummy-wait-lock"))

(let ((event-queue ())
      (lock (make-lock "event-lock")))
  (defun pop-event ()
    "THIS NEEDS TO BE THREADSAFE"
    (with-lock-held (lock)
      (pop event-queue)))

  (defun push-event (item)
    "THIS NEEDS TO BE THREADSAFE"
    (with-lock-held (lock)
      (setf event-queue (append event-queue (list item))))
    (condition-notify *new-events*)))

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
  (loop do (let ((event (pop-event)))
             (if event
               (apply (car event) (cdr event))
               (with-lock-held (*dummy-wait-lock*)
                   (condition-wait *new-events* *dummy-wait-lock*))))))

(defun start-kilns (count)
  (loop for i from 1 to count
    collecting (make-thread #'run-kiln :name (format nil "kiln ~d" i))))

(defun add-process (process kell)
  (when (typep process 'process) ; let us run normal functions without consequence
    (setf (parent process) kell
          (process kell) (compose-processes process (process kell)))
    (mapcar #'push-event (collect-channel-names process kell))))

(defgeneric collect-channel-names (process kell)
  (:documentation "This returns a list of events to add to the event queue.")
  (:method ((process process-variable) (kell kell))
    ;; FIXME: not good enough. Need to prevent it from getting into the kell.
    (warn "Can't have a free variable in an active kell."))
  (:method ((process parallel-composition) (kell kell))
    (apply #'append
           (map-parallel-composition (lambda (proc)
                                       (collect-channel-names proc kell))
                                     process)))
  (:method ((process message) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (messages kell)))
      (list (list #'match-on name 'message kell))))
  (:method ((process kell) (kell kell))
    (let ((name (name process)))
      (push process (gethash name (kells kell)))
      (list (list #'match-on name 'kell kell))))
  (:method ((process trigger) (kell kell))
    (let ((names (channel-names (pattern process)))
          (events ()))
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
      (maphash (lambda (key value)
                 (loop for name in value
                   do (push (list #'match-on name key kell) events)))
               names)
      events))
  (:method ((process (eql ∅)) (kell kell))
    (declare (ignore kell))
    '()))

(defun toplevel (cpu-count)
  (let* ((*top-kell* (make-instance 'kell :name (gensym "LOCALHOST")))
         (kilns (start-kilns cpu-count))
         (*package* (find-package :kilns)) ; FIXME: should use a different package
         (*readtable* *kilns-readtable*))
    ;; dummy kell for now, to handle locking and other places we refer to parents
    (setf (parent *top-kell*) (make-instance 'kell :name (gensym "NETWORK")))
    (unwind-protect
        (loop do
          (princ "> ")
          (let ((process (eval (read))))
            (format t "~a~%" process)
            (add-process process *top-kell*)))
      (mapc #'destroy-thread kilns))))

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

(defmethod trigger-process ((trigger trigger) mapping)
  "Activates process after substituting the process-variables in the trigger."
  (mapc (lambda (process-variable)
          ;; FIXME: this will replace the local instances of the variables, but
          ;;        not ones more deeply nested.
          (warn "Replacing ~a with ~a" process-variable
                (find-process-variable-value process-variable mapping))
          (setf (process trigger)
                (compose-processes (find-process-variable-value process-variable
                                                                mapping)
                                   (process trigger)))
          (remove-process-from process-variable trigger))
        (process-variables-in (process trigger)))
  (remove-process trigger)
  (add-process (process trigger) (parent trigger)))

(defmethod activate-continuation (process)
    (remove-process process)
    (add-process (continuation process) (parent process)))

(defun match-on (name type kell)
  (lock-neighboring-kells (kell)
    (destructuring-bind (trigger matched-process) (really-match-on name type kell)
      (when (and trigger matched-process)
        (format t
                "The pattern ~a will match the process ~a and result in the ~
                 process ~a.~%"
                (pattern trigger) matched-process (process trigger))
        (trigger-process trigger (unify (pattern trigger) matched-process))
        (activate-continuation matched-process)))))

(defgeneric really-match-on (name type kell)
  (:documentation "Tries to find a match for all the patterns that could match
                   channel NAME in KELL.")
  (:method (name (type (eql 'message)) (kell kell))
    ;; FIXME: This should quit after a successful match, I think. If there are
    ;;        more potential matches, there will be more MATCH-ON events in the
    ;;        queue.
    (or (really-match-on name 'local kell)
        (car (mapcar (lambda (subkell) (really-match-on name 'up subkell))
                     (subkells kell)))
        (really-match-on name 'down (parent kell)))) ; FIXME: this could match wrong
  (:method (name (type (eql 'kell)) (kell kell))
    (list (car (gethash name (kell-patterns kell)))
          (car (gethash name (kells kell)))))
  (:method (name (type (eql 'local)) (kell kell))
    (list (car (gethash name (local-patterns kell)))
          (car (gethash name (messages kell)))))
  (:method (name (type (eql 'down)) (kell kell))
    (list (car (gethash name (down-patterns kell)))
          (car (mapcan (lambda (subkell) (gethash name (messages subkell)))
                       (subkells kell)))))
  (:method (name (type (eql 'up)) (kell kell))
    (list (car (gethash name (up-patterns kell)))
          (car (gethash name (messages (parent kell)))))))

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
