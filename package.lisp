(defpackage kilns
  (:documentation
   "A simple and direct implementation of the Kell calculus, suitable for testing
    the behavior of more advanced implementations.")
  (:use #:cl #:bordeaux-threads #:unify)
  (:export #:trigger
           #:restriction
           #:message
           #:kell
           #:∅
           #:def
           ;;; FIXME: most or all of these shouldn't be exported – eventually
           #:pattern
           #:pattern-composition
           #:message-pattern #:down-pattern #:up-pattern #:kell-pattern
           #:parallel-composition #:process-variable))
