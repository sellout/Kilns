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

(defun order-procs (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  (let ((index 0))
    (cons 'par
          (mapcar (lambda (process)
                    `(message ,(incf index) ,process))
                  processes))))

(defun define-definition (pattern &rest processes)
  (destructuring-bind (name &rest parameters) pattern
    (make-instance 'definition
                   :name name
                   :pattern (define-pattern *current-pattern-language*
                                (apply #'order-procs
                                       parameters))
                   :process (define-parallel-composition processes))))

(defun define-named-concretion (name &rest arguments)
  (make-instance 'named-concretion
                 :name name
                 :messages (eval (apply #'order-procs arguments))))

(defun eval (form)
  (if (listp form)
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

;;; FIXME: for networking, load needs to be able to take a path to a subkell
;;;        that represents what is to be run in the local instance. It should
;;;        work as if the following (illegal) trigger were used:
;;;            (trigger [path [to [correct [kell ?process]]]] ?process)
(defun load (file-name
             &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist :error))
  (declare (ignore if-does-not-exist verbose))
  (let ((full-name (merge-pathnames file-name (make-pathname :type "kiln"))))
    (with-open-file (stream full-name :external-format :utf-8)
      (apply #'parallel-composition
             (reverse (loop for value = (read stream nil)
                        while value
                        do (if print (print value) value)
                        collecting value))))))

(defmacro lisp (&rest forms)
  `'(cl:progn ,@forms))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* *kilns-readtable*)
  (values))
