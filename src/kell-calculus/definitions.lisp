(in-package #:kell-calculus)

(defclass definition (pattern-abstraction)
  ((name :initarg :name :reader name)))

(defun order-processes (&rest processes)
  (let ((index 0))
    (reduce #'compose (mapcar (lambda (process)
                                (message (incf index) process))
                              processes))))

(defmacro def ((name &rest parameters) &body body)
  "Allows us to define new operations. It's currently just CL's DEFMACRO."
  `(progn (defmacro ,name (,@parameters)
            ,@body)
          null))

#|
(defmacro def ((name &rest parameters) process)
  `(make-instance 'definition
                  :name ',name
                  :pattern
                  (convert-process-to-pattern (order-processes ,@parameters))
                  :process ,process))
|#

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
            (par abstraction concretion)))
      (par abstraction concretion)))

(defmethod @ ((abstraction definition) (concretion concretion))
  (par abstraction concretion))

(defmethod @ ((abstraction pattern-abstraction) (concretion named-concretion))
  (par abstraction concretion))
