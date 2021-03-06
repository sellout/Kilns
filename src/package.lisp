(defpackage kell-calculus
  (:documentation "A direct model of the kell calculus.")
  ;; FIXME: shouldn't need threads here
  (:use #:cl #:bordeaux-threads #:unify)
  (:shadow #:substitute #:match)
  (:export #:definition #:define
           #:name #:process-variable #:identifier #:name-type
           #:null #:trigger #:new #:par #:def #:cont
           #:process #:message #:kell #:subkells
           #:argument #:state
           #:parallel-composition #:compose
           #:restriction
           #:null-process
           #:pattern-language #:pattern #:up #:down
           #:substitute
           #:match #:match-local #:match-down #:match-up #:match-kell
           #:bound-names #:free-names #:bound-variables #:free-variables
           #:channel-names
           #:local-message-pattern #:down-message-pattern
           #:up-message-pattern #:kell-message-pattern
           #:sub-reduce
           #:abstraction #:concretion #:agent
           #:restriction-abstraction #:pattern-abstraction
           #:kell-abstraction #:application-abstraction
           #:names #:restricted-names #:continuation #:messages
           #:@
           ;; FIXME: shouldn't export, maybe keep in separate package
           #:named-concretion
           #:parent #:deadp #:lock #:map-parallel-composition #:map-process
           #:find-process-variable-value
           #:find-symbol-value
           #:kells #:processes #:triggers #:primitives #:process-variables
           #:remove-process-from
           #:local-patterns #:down-patterns #:up-patterns #:kell-patterns))

(defpackage kilns
  (:documentation
   "A simple and direct implementation of the Kell calculus, suitable for
    testing the behavior of more advanced implementations.")
  (:use #:closer-common-lisp #:bordeaux-threads #:unify
        #:kell-calculus)
  (:shadowing-import-from #:kell-calculus #:substitute #:match)
  (:shadow #:complement #:eval #:load #:read #:read-from-string)
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
           ;; networking
           #:defhost
           ;; interactive commands
           #:load #:move-up #:move-down
           #:system-state #:dump-system-state #:lisp #:watch))

(defpackage kilns-user
  (:use #:kell-calculus #:kilns)
  (:shadowing-import-from #:kell-calculus #:substitute #:match))
