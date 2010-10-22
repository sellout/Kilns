(defpackage kilns
  (:documentation
   "A simple and direct implementation of the Kell calculus, suitable for testing
    the behavior of more advanced implementations.")
  (:use #:cl #:bordeaux-threads #:unify)
  (:shadow #:match #:complement #:load)
  (:export ;; core calculus
           #:trigger
           #:restriction
           #:message #:up #:down
           #:kell
           #:null
           #:parallel-composition
           #:def
           ;; jK pattern language
           #:process-variable
           ;; pnp-jK pattern language
           #:_
           #:name-variable
           ;; fraKtal pattern language
           #:!=
           ;; abbreviated syntax
           #:par #:new
           ;; interactive commands
           #:move-up #:move-down
           ;; uglier bits
           #:load #:lisp #:dev #:state))
