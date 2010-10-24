(load "library/replication")

(trigger* {constant {?name ?value}}
          {name ?value {constant {name ?value}}})
