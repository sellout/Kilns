(in-package kell-calculus)

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
