(defpackage kilns
  (:documentation
   "A simple and direct implementation of the Kell calculus, suitable for testing
    the behavior of more advanced implementations.")
  (:use #:cl #:bordeaux-threads #:unify)
  (:shadow #:match)
  (:export #:trigger
           #:restriction
           #:message #:up #:down
           #:kell
           #:∅
           #:def
           #:parallel-composition #:process-variable))
