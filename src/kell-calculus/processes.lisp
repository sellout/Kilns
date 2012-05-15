#+xcvb (module (:depends-on ("package")))
(in-package :kell-calculus)

(defclass null-process (process)
  ()
  (:documentation "This is effectively nil. Maybe we should just use nil."))

(defmethod print-object ((obj null-process) stream)
  (princ "null" stream))

(defvar +null-process+ (make-instance 'null-process))
(defvar null +null-process+) ; FIXME: remove this var

(defclass trigger (process pattern-abstraction)
  ())

(defmethod print-object ((obj trigger) stream)
  (format stream "(trigger ~s ~s)" (pattern obj) (process obj)))

(defclass restriction (restriction-abstraction process)
  ())

(defmethod print-object ((obj restriction) stream)
  (format stream "(new ~a ~s)" (names obj) (abstraction obj)))

(defclass message (process)
  ((name :initarg :name :accessor name :type name)
   (argument :initarg :argument :initform +null-process+ :type generic-process
             :accessor argument
             :documentation
             "This defaults to nil rather than null so that we can distinguish
              when the argument was omitted, since in some pattern languages an
              omitted argument means 'match anything' rather than 'match
              null'.")
   (continuation :initarg :continuation :initform +null-process+
                 :type (or generic-process symbol)
                 :accessor continuation)))

(defmethod print-object ((obj message) stream)
  (format stream "{~a~:[ ~s~:[ ~s~;~]~;~]}"
          (name obj)
          (and (or (null (argument obj)) (eql (argument obj) +null-process+))
               (eql (continuation obj) +null-process+))
          (argument obj)
          (eql (continuation obj) +null-process+)
          (continuation obj)))

(defclass kell (process)
  ((name :initarg :name :accessor name :type name)
   (state :initarg :state :initform +null-process+ :type generic-process
          :accessor state)
   (continuation :initarg :continuation :initform +null-process+
                 :type generic-process :accessor continuation)
   ;; implementation details
   (lock :reader lock)
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

(defmethod initialize-instance :after ((obj kell) &key &allow-other-keys)
  (setf (slot-value obj 'lock) (make-lock (format nil "kell ~a" (name obj)))))

(defmethod print-object ((obj kell) stream)
  (format stream "[~a ~s~:[ ~s~;~]]"
          (name obj) (state obj)
          (eql (continuation obj) +null-process+) (continuation obj)))

(defclass parallel-composition (process)
  ;; FIXME: I don't think we need process variables here.
  ((process-variables :initarg :process-variables :initform nil :type list
                      :reader process-variables)
   (messages :initarg :messages :initform nil :type list :reader messages)
   (kells :initarg :kells :initform nil :type list :reader kells)
   (triggers :initarg :triggers :initform nil :type list :reader triggers)
   (named-concretions :initarg :named-concretions :initform nil
                      :reader named-concretions)
   (primitives :initarg :primitives :initform nil :type list :reader primitives
               :documentation "This contains lists, strings, numbers, etc., but
                               also restrictions and any other type of process
                               that doesn't need to be singled out."))
  (:documentation
   "A parallel composition is an unordered set of processes. We flatten nested
    compositions and split them up based on their type. This makes for quicker
    access in the way we usually deal with them."))

(defmethod print-object ((obj parallel-composition) stream)
  (format stream "(par~{ ~s~})"
          (map-parallel-composition #'identity obj)))

(defun parallel-composition (&rest processes)
  (reduce #'compose processes :initial-value +null-process+))

(defun map-parallel-composition (fn pc)
  (mapcar fn
          (append (process-variables pc)
                  (messages pc)
                  (kells pc)
                  (triggers pc)
                  (named-concretions pc)
                  (primitives pc))))

;;; FIXME: replace all instances of MAP-PARALLEL-COMPOSITION with this
(defun map-process (fn process)
  "Iterates through all the processes (only one if it's not a
   parallel-composition) and returns a new process as a result."
  (apply #'parallel-composition
         (mapcar fn
                 (append (process-variables-in process)
                         (messages-in process)
                         (kells-in process)
                         (triggers-in process)
                         (named-concretions-in process)
                         (primitives-in process)))))

(defmethod (setf parent) (value (process parallel-composition))
  (setf (slot-value process 'parent) value)
  (map-parallel-composition (lambda (proc)
                              (setf (parent proc) value))
                            process))

(defmethod (setf parent) (value process)
  "Do nothing for primitives."
  (declare (ignore value process))
  (values))

(defgeneric remove-process-from (process kell)
  (:documentation "This mutates a kell by removing the provided process.")
  (:method-combination contract)
  (:method :require "process is in kell" (process (kell kell))
    (let ((state (state kell)))
      (typecase state
        (parallel-composition
         (member process (map-parallel-composition #'identity state)))
        (t (eq process state)))))
  (:method ((process message) (kell kell))
    (let ((state (state kell)))
      (setf (state kell)
            (typecase state
              (message null)
              (parallel-composition
               (apply #'parallel-composition
                      (remove process
                              (map-parallel-composition #'identity state))))))))
  (:method ((process kell) (kell kell))
    (let ((state (state kell)))
      (setf (state kell)
            (typecase state
              (kell null)
              (parallel-composition
               (apply #'parallel-composition
                      (remove process
                              (map-parallel-composition #'identity state))))))))
  (:method ((process trigger) (kell kell))
    (let ((state (state kell)))
      (setf (state kell)
            (typecase state
              (trigger null)
              (parallel-composition
               (apply #'parallel-composition
                      (remove process
                              (map-parallel-composition #'identity state))))))))
  (:method (process (kell kell))
    (let ((state (state kell)))
      (setf (state kell)
            (typecase state
              (parallel-composition
               (apply #'parallel-composition
                      (remove process
                              (map-parallel-composition #'identity state))))
              (t null))))))

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
  (kells-in (state kell)))

(defmethod compose (process-a process-b)
  (compose (compose (make-instance 'parallel-composition) process-a)
           process-b))
(defmethod compose ((process-a process) (process-b process))
  (compose (compose (make-instance 'parallel-composition) process-a)
           process-b))
(defmethod compose ((process-a null-process) process-b)
  process-b)
(defmethod compose (process-a (process-b null-process))
  process-a)
(defmethod compose
    ((process-a parallel-composition) (process-b parallel-composition))
  (make-instance 'parallel-composition
                 :process-variables (append (process-variables process-a)
                                            (process-variables process-b))
                 :messages (append (messages process-a) (messages process-b))
                 :kells (append (kells process-a) (kells process-b))
                 :triggers (append (triggers process-a) (triggers process-b))
                 :named-concretions (append (named-concretions process-a)
                                            (named-concretions process-b))
                 :primitives (append (primitives process-a)
                                     (primitives process-b))))
(defmethod compose (process-a (process-b parallel-composition))
  "This method just swaps the args so the function is commutatitve."
  (compose process-b process-a))
(defmethod compose ((process-a parallel-composition) (process-b null-process))
  process-a)
(defmethod compose ((process-a parallel-composition) process-b)
  (make-instance 'parallel-composition
                 :process-variables (process-variables process-a)
                 :messages (messages process-a)
                 :kells (kells process-a)
                 :triggers (triggers process-a)
                 :named-concretions (named-concretions process-a)
                 :primitives (cons process-b (primitives process-a))))
(defmethod compose ((process-a parallel-composition) (process-b message))
  (make-instance 'parallel-composition
                 :process-variables (process-variables process-a)
                 :messages (cons process-b (messages process-a))
                 :kells (kells process-a)
                 :triggers (triggers process-a)
                 :named-concretions (named-concretions process-a)
                 :primitives (primitives process-a)))
(defmethod compose ((process-a parallel-composition) (process-b kell))
  (make-instance 'parallel-composition
                 :process-variables (process-variables process-a)
                 :messages (messages process-a)
                 :kells (cons process-b (kells process-a))
                 :triggers (triggers process-a)
                 :named-concretions (named-concretions process-a)
                 :primitives (primitives process-a)))
(defmethod compose
    ((process-a parallel-composition) (process-b process-variable))
  (make-instance 'parallel-composition
                 :process-variables (cons process-b
                                          (process-variables process-a))
                 :messages (messages process-a)
                 :kells (kells process-a)
                 :triggers (triggers process-a)
                 :named-concretions (named-concretions process-a)
                 :primitives (primitives process-a)))
(defmethod compose ((process-a parallel-composition) (process-b trigger))
  (make-instance 'parallel-composition
                 :process-variables (process-variables process-a)
                 :messages (messages process-a)
                 :kells (kells process-a)
                 :triggers (cons process-b (triggers process-a))
                 :named-concretions (named-concretions process-a)
                 :primitives (primitives process-a)))

(defun null-name ()
  (break "nil name!"))

(defmethod initialize-instance :after
    ((instance message) &key name &allow-other-keys)
  (when (null name)
    (null-name)))

(trace :before :backtrace null-name)
