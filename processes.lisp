#+xcvb (module (:depends-on ("package")))
(in-package :kilns)

(defclass process ()
  ((parent :accessor parent))) ; FIXME: really only a property of _active_ processes …

(defclass null-process (process)
  ()
  (:documentation "This is effectively nil. Maybe we should just use nil."))

(defmethod print-object ((obj null-process) stream)
  (princ "∅" stream))

(defvar ∅ (make-instance 'null-process))

(defclass process-variable (process)
  ((name :initarg :name :type symbol :reader name))
  (:documentation
   "These only exist in “potential” processes. When a trigger is triggered, we
    convert each process-variable into its “realized” process and do so
    recursively through nested processes, except where the variable is shadowed
    by a more local variable with the same name."))

(defmethod print-object ((obj process-variable) stream)
  (format stream "?~a" (name obj)))

(defun process-variable (name)
  (make-instance 'process-variable :name name))

(defclass trigger (process)
  ((pattern :initarg :pattern :type pattern :accessor pattern)
   (process :initarg :process :type process :accessor process)))

(defmethod print-object ((obj trigger) stream)
  (if (and (slot-boundp obj 'pattern)
           (slot-boundp obj 'process))
    (format stream "(~a ▹ ~a)" (pattern obj) (process obj))
    (call-next-method)))

(defun trigger (pattern process)
  (make-instance 'trigger
    :pattern (convert-process-to-pattern pattern) :process process))

(defclass restriction (process)
  ((name :initarg :name :type name :reader name)
   (process :initarg :process :type process :reader process))
  (:documentation
   "We store everything in normal form, which means that restrictions don't
    actually exist, per se. They are all brought to the top-level as global
    channels with uniqified names (and the original name retained as a
   “nickname”). This also simplifies the communication of restricted channels, as
    only the unique name needs to be shared, and no handling of scope needs to be
    managed."))

(defmethod print-object ((obj restriction) stream)
  (format stream "(ν~a.~a)" (name obj) (process obj)))

(defun restriction (name process)
  (make-instance 'restriction :name name :process process))

;;; FIXME: need  a better name
(defclass message-structure (process)
  ((name :initarg :name :type name :accessor name)
   (process :initarg :process :initform ∅ :type process :accessor process)
   (continuation :initarg :continuation :initform ∅
                 :type (or process (member up down))
                 :reader continuation))
  (:documentation "The commonalities between messages and kells."))

(defclass message (message-structure)
  ())

(defmethod print-object ((obj message) stream)
  (if (and (slot-boundp obj 'name)
           (slot-boundp obj 'process)
           (slot-boundp obj 'continuation))
    (format stream "~a~:[〈~a〉~:[~a~;~]~;~]"
            (name obj)
            (and (eql (process obj) ∅) (eql (continuation obj) ∅)) (process obj)
            (eql (continuation obj) ∅)
            (case (continuation obj)
              (down "↓")
              (up "↑")
              (otherwise (format nil ".~a" (continuation obj)))))
    (call-next-method)))

(defun message (name &optional process continuation)
  (if continuation
    (make-instance 'message
      :name name :process process :continuation continuation)
    (if process
      (make-instance 'message :name name :process process)
      (make-instance 'message :name name))))

(defclass kell (message-structure)
  (;; implementation details
   (lock :initform (make-lock) :reader lock)
   ;; while each trigger pattern contains a multiset of typed patterns, we keep
   ;; them aggregated here in a hash-table mapping the channel-name to the
   ;; trigger that they are a part of. This makes the matching much easier, but
   ;; it means we have to keep the data structures in sync.
   (messages :initform (make-hash-table :test #'equal) :reader messages)
   (kells :initform (make-hash-table :test #'equal) :reader kells)
   (local-patterns :initform (make-hash-table :test #'equal) :reader local-patterns)
   (up-patterns :initform (make-hash-table :test #'equal) :reader up-patterns)
   (down-patterns :initform (make-hash-table :test #'equal) :reader down-patterns)
   (kell-patterns :initform (make-hash-table :test #'equal) :reader kell-patterns)))

(defun kell (name &optional process continuation)
  (if continuation
    (make-instance 'kell :name name :process process :continuation continuation)
    (if process
      (make-instance 'kell :name name :process process)
      (make-instance 'kell :name name))))

(defmethod print-object ((obj kell) stream)
  (format stream "~a[~a]~:[.~a~;~]"
          (name obj) (process obj)
          (eql (continuation obj) ∅) (continuation obj)))

(defclass parallel-composition (process)
  ((process-variables :initform nil :type list :accessor process-variables)
   (messages :initform nil :type list :accessor messages)
   (kells :initform nil :type list :accessor kells)
   (triggers :initform nil :type list :accessor triggers))
  (:documentation
   "A parallel composition is an unordered set of processes. We flatten nested
    compositions and split them up based on their type. This makes for quicker
    access in the way we usually deal with them."))

(defmethod print-object ((obj parallel-composition) stream)
  (format stream "~{~a~^ | ~}"
          (map-parallel-composition #'identity obj)))

(defun parallel-composition (&rest processes)
  (reduce #'compose-processes processes))

(defun map-parallel-composition (fn pc)
  (append (mapcar fn (process-variables pc))
          (mapcar fn (messages pc))
          (mapcar fn (kells pc))
          (mapcar fn (triggers pc))))

(defmethod (setf parent) (value (process parallel-composition))
  (setf (slot-value process 'parent) value)
  (map-parallel-composition (lambda (proc)
                              (setf (parent proc) value))
                            process))


(defgeneric remove-process-from (process kell)
  (:method (process kell)
    (warn "The process ~a is not contained in ~a, and thus can not be removed."
          process (process kell)))
  (:method ((process process-variable) trigger)
    (if (find process (process-variables-in (process trigger)))
      (typecase (process trigger)
        (process-variable (setf (process trigger) ∅))
        (parallel-composition (setf (process-variables (process trigger))
                                    (delete process
                                            (process-variables (process trigger))))))
      (warn "The process ~a is not contained in ~a, and thus can not be removed."
            process (process trigger))))
  (:method ((process message) kell)
    (if (find process (messages-in (process kell)))
      (typecase (process kell)
        (message (setf (process kell) ∅))
        (parallel-composition (setf (messages (process kell))
                                    (delete process (messages (process kell))))))
      (warn "The process ~a is not contained in ~a, and thus can not be removed."
            process (process kell))))
  (:method ((process kell) kell)
    (if (find process (kells-in (process kell)))
      (typecase (process kell)
        (kell (setf (process kell) ∅))
        (parallel-composition (setf (kells (process kell))
                                    (delete process (kells (process kell))))))
      (warn "The process ~a is not contained in ~a, and thus can not be removed."
            process (process kell))))
  (:method ((process trigger) kell)
    (if (find process (triggers-in (process kell)))
      (typecase (process kell)
        (trigger (setf (process kell) ∅))
        (parallel-composition (setf (triggers (process kell))
                                    (delete process (triggers (process kell))))))
      (warn "The process ~a is not contained in ~a, and thus can not be removed."
            process (process kell)))))

(defgeneric process-variables-in (process)
  (:documentation
   "Retrieves a list of the process-variables contained in the process.")
  (:method (process)
    (declare (ignore process))
    nil)
  (:method ((process process-variable))
    (list process))
  (:method ((process parallel-composition))
    (process-variables process)))

(defgeneric messages-in (process)
  (:documentation "Retrieves a list of the messages contained in the process.")
  (:method (process)
    (declare (ignore process))
    nil)
  (:method ((process message))
    (list process))
  (:method ((process parallel-composition))
    (messages process)))

(defgeneric kells-in (process)
  (:documentation "Retrieves a list of the kells contained in the process.")
  (:method (process)
    (declare (ignore process))
    nil)
  (:method ((process kell))
    (list process))
  (:method ((process parallel-composition))
    (kells process)))

(defgeneric triggers-in (process)
  (:documentation "Retrieves a list of the triggers contained in the process.")
  (:method (process)
    (declare (ignore process))
    nil)
  (:method ((process trigger))
    (list process))
  (:method ((process parallel-composition))
    (triggers process)))

(defmethod subkells ((kell kell))
  (kells-in (process kell)))

(defgeneric compose-processes (process-a process-b)
  (:documentation "This is destructive.")
  (:method (process-a process-b)
    (let ((pc (make-instance 'parallel-composition)))
      (compose-processes (compose-processes pc process-a)
                         process-b)))
  (:method ((process-a (eql ∅)) process-b)
    process-b)
  (:method (process-a (process-b (eql ∅)))
    process-a)
  (:method ((process-a parallel-composition) (process-b parallel-composition))
    (psetf (process-variables process-a)
           (nconc (process-variables process-a) (process-variables process-b))
           (messages process-a)
           (nconc (messages process-a) (messages process-b))
           (kells process-a)
           (nconc (kells process-a) (kells process-b))
           (triggers process-a)
           (nconc (triggers process-a) (triggers process-b)))
    process-a)
  (:method (process-a (process-b parallel-composition))
    "This method just swaps the args so the function is commutatitve."
    (compose-processes process-b process-a))
  ;; FIXME: I think we don't need this method
  ;;(:method ((process-a parallel-composition) process-b)
  ;;  "This method catches us to avoid infinite recursion."
  ;;  (error "~a is not a valid process." process-b))
  (:method ((process-a parallel-composition) (process-b message))
    (push process-b (messages process-a))
    process-a)
  (:method ((process-a parallel-composition) (process-b kell))
    (push process-b (kells process-a))
    process-a)
  (:method ((process-a parallel-composition) (process-b process-variable))
    (push process-b (process-variables process-a))
    process-a)
  (:method ((process-a parallel-composition) (process-b trigger))
    (push process-b (triggers process-a))
    process-a))

#| not sure if we ever need this – annotated messages aren't a thing so much as a category
(defmethod annotated-messages ((kell kell))
  "Collects the set of local, up, down, and subkell messages for the given kell."
  (append (messages kell)
          (mapcar (lambda (message) (annotate-message message 'up))
                  (messages (parent kell)))
          (mapcar (lambda (message) (annotate-message message 'down))
                  (mapcan #'messages (subkells kell)))
          (subkells kell)))
|#

#| new definition in syntax.lisp
(defgeneric add-process (kell process)
  (:documentation "Adds a new process to a kell and triggers an attempt to match
                  on the new process."
  (:method :before (kell process)
    (push-process process kell))
  (:method ((kell kell) (process trigger))
    (match (pattern trigger) (annotated-messages kell)))
  (:method ((kell kell) (process message))
    (match (patterns kell) (annotated-messages kell)))
  (:method ((kell kell) (process parallel-composition))
    (match (patterns kell) (annotated-messages kell)))))
|#