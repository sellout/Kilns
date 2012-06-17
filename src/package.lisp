(defpackage kell-calculus
  (:documentation "A direct model of the kell calculus.")
  ;; FIXME: shouldn't need threads here
  (:use #:cl #:bordeaux-threads #:unify #:quid-pro-quo)
  (:shadow #:substitute #:match)
  (:export #:definition #:define
           #:name #:process-variable #:identifier #:name-type
           #:null #:+null-process+ #:trigger #:new #:par #:def #:cont
           #:process #:message #:kell #:subkells
           #:argument #:state
           #:parallel-composition #:compose
           #:restriction
           #:null-process
           #:pattern-language #:*current-pattern-language* #:pattern #:up #:down
           #:substitute
           #:match #:match-local #:match-down #:match-up #:match-kell
           #:bound-names #:free-names #:bound-variables #:free-variables
           #:channel-names
           #:local-message-pattern #:down-message-pattern #:placeholders
           #:up-message-pattern #:kell-message-pattern #:named-concretions
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
  (:use #:closer-common-lisp #:bordeaux-threads #:unify #:quid-pro-quo
        #:kell-calculus)
  (:shadowing-import-from #:kell-calculus #:substitute #:match)
  (:shadow #:complement #:eval #:load #:read #:read-from-string)
  (:export ;; core calculus
           #:eval
           #:trigger
           #:restriction
           #:message #:up #:down
           #:kell
           #:null
           #:parallel-composition
           #:def
           #:*current-pattern-language*
           #:+jk-calculus+ ; jK pattern language
           #:process-variable
           #:+pnpjk-calculus+ ; pnp-jK pattern language
           #:+blank+
           #:name-variable
           #:+fraktal+ ; fraKtal pattern language
           ;; abbreviated syntax
           #:par #:new #:!=
           ;; networking
           #:defhost
           ;; running
           #:start #:stop #:toplevel #:run-toplevel
           ;; interactive commands
           #:load #:move-up #:move-down
           #:system-state #:dump-system-state #:watch
           #:lisp #:substitute-variables))

(defpackage kilns-user
  (:use #:kell-calculus #:kilns))
