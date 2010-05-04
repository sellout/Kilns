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
  (:method (local-name global-name (process trigger))
    (apply-restriction local-name global-name (pattern process))
    (apply-restriction local-name global-name (process process))
    process))


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

(defgeneric really-match-on (process kell)
  (:documentation "Tries to find a match for all the patterns that could match
                   channel NAME in KELL.")
  (:method ((process message) (kell kell))
    "Find all triggers that could match – up, down, or local."
    (let ((name (name process)))
      (catch 'match
        (mapc (lambda (trigger)
                (format t "attempt to match ~a against ~a~%" (pattern trigger) kell)
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
              (format t "attempt to match ~a against ~a~%" (pattern trigger) kell)
              (handler-case
                  (destructuring-bind (processes substitutions)
                                      (match (pattern trigger) kell)
                    (throw 'match (list trigger processes substitutions)))
                (error () nil)))
            (gethash (name process) (kell-patterns kell)))))
  (:method ((process trigger) (kell kell))
    "Just match on the new trigger."
    (format t "attempt to match ~a against ~a~%" (pattern process) kell)
    (handler-case
        (destructuring-bind (processes substitutions)
                            (match (pattern process) (parent process))
          (list process processes substitutions))
      (error () nil))))

(defun match-on (process kell)
  (lock-neighboring-kells (kell)
    (destructuring-bind (&optional trigger matched-processes substitutions)
                        (really-match-on process kell)
      (when (and trigger matched-processes)
        (format t
                "The pattern ~a will match the process ~a and result in the ~
                 process ~a.~%"
                (pattern trigger) matched-processes (process trigger))
        (trigger-process trigger substitutions)
        (mapc #'activate-continuation matched-processes)))))

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
