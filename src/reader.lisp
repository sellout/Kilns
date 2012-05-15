(in-package #:kilns)

(defun set-egal (left right)
  (null (set-exclusive-or left right :test #'egal)))

(defgeneric egal (x y)
  (:documentation "Henry Baker's EGAL, applied to Kilns.")
  (:method (x y)
    (eq x y))
  (:method ((x symbol) (y symbol))
    (equal (symbol-name x) (symbol-name y)))
  (:method ((x message) (y message))
    (and (egal (name x) (name y))
         (egal (argument x) (argument y))
         (egal (continuation x) (continuation y))))
  (:method ((x kell) (y kell))
    (and (egal (name x) (name y))
         (egal (state x) (state y))
         (egal (continuation x) (continuation y))))
  (:method ((x parallel-composition) (y parallel-composition))
    (set-egal (map-parallel-composition #'identity x)
              (map-parallel-composition #'identity y)))
  (:method ((x restriction) (y restriction))
    (and (set-egal (names x) (names y))
         (egal (abstraction x) (abstraction y))))
  (:method ((x trigger) (y trigger))
    (and (egal (pattern x) (pattern y))
         (egal (process x) (process y))))
  (:method ((x pattern) (y pattern))
    (and (set-egal (local-message-pattern x) (local-message-pattern y))
         (set-egal (down-message-pattern x) (down-message-pattern y))
         (set-egal (up-message-pattern x) (up-message-pattern y))
         (set-egal (kell-message-pattern x) (kell-message-pattern y)))))

(defgeneric define-pattern (pattern-language pattern)
  (:documentation "This needs to be defined for each pattern language. It
                   determines how the patterns are interpreted."))

(defun define-kell
    (name &rest state)
  (make-instance 'kell
                 :name (eval name)
                 :state (if state
                            (define-parallel-composition state)
                            +null-process+)))

(defun define-message (name &rest argument)
  (make-instance 'message
                 :name (eval name)
                 :argument (if argument
                               (define-parallel-composition argument)
                               +null-process+)))

(defun define-parallel-composition (processes)
  (reduce #'compose (mapcar #'eval processes) :initial-value +null-process+))

(defun define-restriction (names &rest processes)
  (make-instance 'restriction
                 :names (if (listp names)
                            (mapcar #'eval names)
                            (list (eval names)))
                 :abstraction (define-parallel-composition processes)))

(defun define-trigger (pattern &rest processes)
  (make-instance 'trigger
                 :pattern (define-pattern *current-pattern-language* pattern)
                 :process (define-parallel-composition processes)))

(defun order-forms (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  (let ((index 0))
    (cons 'par
          (mapcar (lambda (process)
                    `(message ,(incf index) ,process))
                  processes))))

(defgeneric count-concretions (process)
  (:documentation "This allows us to decide whether a particular object is a
                   process or a pattern.")
  (:method (process)
    0)
  (:method ((process concretion))
    (1+ (count-concretions (messages process))))
  (:method ((process message))
    (+ (count-concretions (argument process))
       (count-concretions (continuation process))))
  (:method ((process kell))
    (+ (count-concretions (state process))
       (count-concretions (continuation process))))
  (:method ((process parallel-composition))
    (reduce #'+ (map-parallel-composition #'count-concretions process)))
  (:method ((process trigger))
    "If it has a trigger, it must be a process."
    0)
  (:method ((process restriction))
    "If it has a restriction, it must be a process."
    0)
  (:method ((process pattern))
    (+ (count-concretions (local-message-pattern process))
       (count-concretions (up-message-pattern process))
       (count-concretions (down-message-pattern process))
       (count-concretions (kell-message-pattern process))
       (count-concretions (named-concretions process)))))

(defun order-procs (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  ;; TODO: This currently tries to parse as both a process and a pattern, and
  ;;      (if both succeed) it chooses the one with fewer concretions (since
  ;;       various special forms would be read as concretions in the wrong
  ;;       context).
  (let ((index 0))
    (apply #'parallel-composition
           (mapcar (lambda (process)
                     (let ((new-proc (ignore-errors (eval process)))
                           (new-patt (ignore-errors (define-pattern *current-pattern-language*
                                                                    process)))
                           (new-patt-component (ignore-errors (define-pattern-message-argument *current-pattern-language*
                                                                  process))))
                       (make-instance 'message
                                      :name (incf index)
                                      :argument (car (stable-sort (remove nil
                                                                          (list new-proc new-patt-component new-patt))
                                                                  #'<
                                                                  :key #'count-concretions)))))
                   processes))))

(defun define-definition (pattern &rest processes)
  (destructuring-bind (name &rest parameters) pattern
    (setf (gethash name *global-definitions*)
          (make-instance 'definition
                         :name name
                         :pattern (define-pattern *current-pattern-language*
                                      (apply #'order-forms parameters))
                         :process (define-parallel-composition processes))))
  +null-process+)

(defun define-named-concretion (name &rest arguments)
  "Even though this is in a process, it could match a definition where it
   expands into pattern position, so we try to patternize it if need be."
  (make-instance 'named-concretion
                 :name (if (listp name)
                           (define-parallel-composition (list name))
                           name)
                 :messages (apply #'order-procs arguments)))

(defun eval (form)
  (if (consp form)
      (case (car form)
        (cont (let ((process (eval (second form))))
                (setf (continuation process)
                      (define-parallel-composition (cddr form)))
                process))
        (kell (apply #'define-kell (cdr form)))
        (message (apply #'define-message (cdr form)))
        (new (apply #'define-restriction (cdr form)))
        (par (define-parallel-composition (cdr form)))
        (trigger (apply #'define-trigger (cdr form)))
        (define (apply #'define-definition (cdr form)))
        ((process-variable name-variable)
         (error "Not a valid process."))
        (otherwise (apply #'define-named-concretion form)))
      (if (and (not *reading-name-p*) (eq form 'null))
          +null-process+
          form)))

(defun read (&rest read-args)
  (let* ((*readtable* kilns::*kilns-readtable*)
         (*package* (find-package :kilns-user))
         (*read-eval* nil)
         (value (apply #'cl:read read-args)))
    (case value
      (null null)
      (otherwise value))))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace
                                &aux idx)
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (values
   (with-input-from-string (stream string :index idx :start start :end end)
     (if preserve-whitespace
       (read-preserving-whitespace stream eof-error-p eof-value)
       (read stream eof-error-p eof-value)))
   idx))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* *kilns-readtable*)
  (values))
