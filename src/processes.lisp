#+xcvb (module (:depends-on ("package")))
(in-package :kilns)

(defclass process ()
  ;; FIXME: really only a property of _active_ processes …
  ((parent :accessor parent)))

(deftype generic-process ()
  "This allows us to use various primitives as processes."
  `(or process string number list))

(defclass null-process (process)
  ()
  (:documentation "This is effectively nil. Maybe we should just use nil."))

(defmethod print-object ((obj null-process) stream)
  (princ "null" stream))

(defvar null-process (make-instance 'null-process))

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

(defclass name-variable ()
  ((name :initarg :name :type symbol :reader name))
  (:documentation
   "These only exist in “potential” processes. When a trigger is triggered, we
    convert each name-variable into its “realized” name and do so
    recursively through nested processes, except where the variable is shadowed
    by a more local variable with the same name."))

(defmethod print-object ((obj name-variable) stream)
  (format stream "?~a" (name obj)))

(defun name-variable (name)
  (make-instance 'name-variable :name name))

(defclass trigger (process)
  ((pattern :initarg :pattern :type pattern :accessor pattern)
   (process :initarg :process :type generic-process :accessor process)))

(defmethod print-object ((obj trigger) stream)
  (if (and (slot-boundp obj 'pattern)
           (slot-boundp obj 'process))
    (format stream "(trigger ~a ~a)" (pattern obj) (process obj))
    (call-next-method)))

(defun trigger (pattern process)
  (make-instance 'trigger
    :pattern (convert-process-to-pattern pattern) :process process))

(defclass restriction (process)
  ((name :initarg :name :type name :reader name)
   (process :initarg :process :type generic-process :accessor process))
  (:documentation
   "We store everything in normal form, which means that restrictions don't
    actually exist, per se. They are all brought to the top-level as global
    channels with uniqified names (and the original name retained as a
   “nickname”). This also simplifies the communication of restricted channels,
    as only the unique name needs to be shared, and no handling of scope needs
    to be managed."))

(defmethod print-object ((obj restriction) stream)
  (format stream "(new ~a ~a)" (name obj) (process obj)))

(defun restriction (name process)
  (make-instance 'restriction :name name :process process))

;;; FIXME: need  a better name
(defclass message-structure (process)
  ((name :initarg :name :type name :accessor name)
   (process :initarg :process :initform null-process :type generic-process
            :accessor process)
   (continuation :initarg :continuation :initform null-process
                 :type (or process (member up down))
                 :accessor continuation))
  (:documentation "The commonalities between messages and kells."))

(defclass message (message-structure)
  ())

(defmethod print-object ((obj message) stream)
  (if (and (slot-boundp obj 'name)
           (slot-boundp obj 'process)
           (slot-boundp obj 'continuation))
    (format stream "{~a~:[ ~a~:[ ~a~;~]~;~]}"
            (name obj)
            (and (eql (process obj) null-process)
                 (eql (continuation obj) null-process))
            (process obj)
            (eql (continuation obj) null-process)
            (continuation obj))
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
   (local-patterns :initform (make-hash-table :test #'equal)
                   :reader local-patterns)
   (up-patterns :initform (make-hash-table :test #'equal) :reader up-patterns)
   (down-patterns :initform (make-hash-table :test #'equal)
                  :reader down-patterns)
   (kell-patterns :initform (make-hash-table :test #'equal)
                  :reader kell-patterns)))

(defun kell (name &optional process continuation)
  (if continuation
    (make-instance 'kell :name name :process process :continuation continuation)
    (if process
      (make-instance 'kell :name name :process process)
      (make-instance 'kell :name name))))

(defmethod print-object ((obj kell) stream)
  (format stream "[~a ~a~:[ ~a~;~]]"
          (name obj) (process obj)
          (eql (continuation obj) null-process) (continuation obj)))

(defclass parallel-composition (process)
  ((process-variables :initform nil :type list :accessor process-variables)
   (messages :initform nil :type list :accessor messages)
   (kells :initform nil :type list :accessor kells)
   (triggers :initform nil :type list :accessor triggers)
   (primitives :initform nil :type list :accessor primitives
               :documentation "This contains lists, strings, numbers, etc., but
                               also restrictions and any other type of process
                               that doesn't need to be singled out."))
  (:documentation
   "A parallel composition is an unordered set of processes. We flatten nested
    compositions and split them up based on their type. This makes for quicker
    access in the way we usually deal with them."))

(defmethod print-object ((obj parallel-composition) stream)
  (format stream "(par~{ ~a~})"
          (map-parallel-composition #'identity obj)))

(defun parallel-composition (&rest processes)
  (reduce #'compose-processes processes))

(defun map-parallel-composition (fn pc)
  (append (mapcar fn (process-variables pc))
          (mapcar fn (messages pc))
          (mapcar fn (kells pc))
          (mapcar fn (triggers pc))
          (mapcar fn (primitives pc))))

(defmethod (setf parent) (value (process parallel-composition))
  (setf (slot-value process 'parent) value)
  (map-parallel-composition (lambda (proc)
                              (setf (parent proc) value))
                            process))

(defmethod (setf parent) (value process)
  "Do nothing for primitives."
  (values))

;;; FIXME: there are really two functions here – one replaces the value in place
;;;        in the parent container, the other is functional. The functional one
;;;        is right, but we have to convert other code.
(defgeneric remove-process-from (process kell)
  (:method ((process process-variable) (par parallel-composition))
    (if (find process (process-variables-in par))
      (progn
        (setf (process-variables par)
              (delete process (process-variables par)))
        (case (length (map-parallel-composition #'identity par))
          (0 null-process)
          (1 (car (map-parallel-composition #'identity par)))
          (otherwise par)))
      (error "The (variable) process ~a is not contained in ~a, and thus can ~
              not be removed."
             process par)))
  (:method ((process process-variable) (var process-variable))
    (if (find process (process-variables-in var))
      null-process
      (error "The (variable) process ~a is not contained in ~a, and thus can ~
              not be removed."
             process var)))
  (:method ((process process-variable) trigger)
    (if (find process (process-variables-in (process trigger)))
      (typecase (process trigger)
        (process-variable (setf (process trigger) null-process))
        (parallel-composition (setf (process-variables (process trigger))
                                    (delete process
                                            (process-variables
                                             (process trigger))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (process trigger)))
                                (0 (setf (process trigger) null-process))
                                (1 (setf (process trigger)
                                         (car (map-parallel-composition
                                               #'identity
                                               (process trigger))))))))
      (error "The (variable) process ~a is not contained in ~a, and thus can ~
              not be removed."
             process (process trigger))))
  (:method ((process message) kell)
    (if (find process (messages-in (process kell)))
      (typecase (process kell)
        (message (setf (process kell) null-process))
        (parallel-composition (setf (messages (process kell))
                                    (delete process (messages (process kell))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (process kell)))
                                (0 (setf (process kell) null-process))
                                (1 (setf (process kell)
                                         (car (map-parallel-composition
                                               #'identity
                                               (process kell))))))))
      (error "The (message) process ~a is not contained in ~a, and thus can ~
              not be removed."
             process (process kell))))
  (:method ((process kell) kell)
    (if (find process (kells-in (process kell)))
      (typecase (process kell)
        (kell (setf (process kell) null-process))
        (parallel-composition (setf (kells (process kell))
                                    (delete process (kells (process kell))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (process kell)))
                                (0 (setf (process kell) null-process))
                                (1 (setf (process kell)
                                         (car (map-parallel-composition
                                               #'identity
                                               (process kell))))))))
      (error "The (kell) process ~a is not contained in ~a, and thus can not ~
              be removed."
             process (process kell))))
  (:method ((process trigger) kell)
    (if (find process (triggers-in (process kell)))
      (typecase (process kell)
        (trigger (setf (process kell) null-process))
        (parallel-composition (setf (triggers (process kell))
                                    (delete process (triggers (process kell))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (process kell)))
                                (0 (setf (process kell) null-process))
                                (1 (setf (process kell)
                                         (car (map-parallel-composition
                                               #'identity
                                               (process kell))))))))
      (error "The (trigger) process ~a is not contained in ~a, and thus can ~
              not be removed."
             process (process kell))))
  (:method (process kell)
    (if (find process (primitives-in (process kell)))
      (typecase (process kell)
        (parallel-composition (setf (primitives (process kell))
                                    (delete process
                                            (primitives (process kell))))
                              (case (length (map-parallel-composition
                                             #'identity
                                             (process kell)))
                                (0 (setf (process kell) null-process))
                                (1 (setf (process kell)
                                         (car (map-parallel-composition
                                               #'identity
                                               (process kell)))))))
        (t (setf (process kell) null-process)))
      (error "The (restriction) process ~a is not contained in ~a, and thus ~
              can not be removed."
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

(defgeneric primitives-in (process)
  (:documentation
   "Retrieves a list of the primitives contained in the process.")
  (:method (process)
    (list process))
  (:method ((process process))
    nil)
  (:method ((process restriction))
    (list process))
  (:method ((process parallel-composition))
    (primitives process)))

(defmethod subkells ((kell kell))
  (kells-in (process kell)))

(defgeneric compose-processes (process-a process-b)
  (:documentation
   "Conses up a new parallel composition that contains both process-a and
    process-b.")
  (:method (process-a process-b)
    (let ((pc (make-instance 'parallel-composition)))
      (compose-processes (compose-processes pc process-a)
                         process-b)))
  (:method ((process-a null-process) process-b)
    process-b)
  (:method (process-a (process-b null-process))
    process-a)
  (:method ((process-a parallel-composition) (process-b parallel-composition))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc)
             (append (process-variables process-a)
                     (process-variables process-b))
             (messages pc)
             (append (messages process-a) (messages process-b))
             (kells pc)
             (append (kells process-a) (kells process-b))
             (triggers pc)
             (append (triggers process-a) (triggers process-b))
             (primitives pc)
             (append (primitives process-a) (primitives process-b)))
      pc))
  (:method (process-a (process-b parallel-composition))
    "This method just swaps the args so the function is commutatitve."
    (compose-processes process-b process-a))
  (:method ((process-a parallel-composition) process-b)
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc) (process-variables process-a)
             (messages pc) (messages process-a)
             (kells pc) (kells process-a)
             (triggers pc) (triggers process-a)
             (primitives pc) (cons process-b (primitives process-a)))
      pc))
  (:method ((process-a parallel-composition) (process-b message))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc) (process-variables process-a)
             (messages pc) (cons process-b (messages process-a))
             (kells pc) (kells process-a)
             (triggers pc) (triggers process-a)
             (primitives pc) (primitives process-a))
      pc))
  (:method ((process-a parallel-composition) (process-b kell))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc) (process-variables process-a)
             (messages pc) (messages process-a)
             (kells pc) (cons process-b (kells process-a))
             (triggers pc) (triggers process-a)
             (primitives pc) (primitives process-a))
      pc))
  (:method ((process-a parallel-composition) (process-b process-variable))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc) (cons process-b
                                          (process-variables process-a))
             (messages pc) (messages process-a)
             (kells pc) (kells process-a)
             (triggers pc) (triggers process-a)
             (primitives pc) (primitives process-a))
      pc))
  (:method ((process-a parallel-composition) (process-b trigger))
    (let ((pc (make-instance 'parallel-composition)))
      (psetf (process-variables pc) (process-variables process-a)
             (messages pc) (messages process-a)
             (kells pc) (kells process-a)
             (triggers pc) (cons process-b (triggers process-a))
             (primitives pc) (primitives process-a))
      pc)))