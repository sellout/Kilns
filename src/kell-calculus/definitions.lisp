(in-package #:kell-calculus)

(defvar *global-definitions* (make-hash-table :test #'eq))

(defclass definition (pattern-abstraction)
  ((name :initarg :name :reader name)))

(defun order-processes (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  (let ((index 0))
    (reduce #'compose
            (mapcar (lambda (process) (make-instance 'message
                                                     :name (incf index)
                                                     :argument process))
                    processes))))

(defmacro def ((name &rest parameters) &body body)
  "Allows us to define new operations. It's currently just CL's DEFMACRO."
  `(progn (defmacro ,name (,@parameters)
            ,@body)
          null))

(defclass named-concretion (concretion)
  ((name :initarg :name :reader name)))

#| This is what the list reader should be
(if (find (car list) '(trigger def par))
    `,list
    `(make-instance 'named-concretion
                    :name ,(car list) :messages '(list ,@(cdr list))))
|#

(defmethod @ ((abstraction definition) (concretion named-concretion))
  (if (eq (name abstraction) (name concretion))
      (let ((substitutions (match (pattern abstraction) (messages concretion))))
        (if substitutions
            (compose (substitute (process abstraction) substitutions)
                     (continuation concretion))
            (compose abstraction concretion)))
      (compose abstraction concretion)))

(defmethod @ ((abstraction definition) (concretion concretion))
  (compose abstraction concretion))

(defmethod @ ((abstraction pattern-abstraction) (concretion named-concretion))
  (compose abstraction concretion))
