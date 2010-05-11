;;; boolean operators that execute all tests fully
(trigger* <all {?test-a ?test-b ?r}>
          (new (k)
               {[k {}]
                (trigger <-resp ?x> {(trigger [k] nil)
                                     <?r ?x>}}


;;; To model early termination, you need to keep each parallel computation in a
;;; separate kell, and have each passivate the others upon failure
;;;
;;; I think you need a macro to handle this for variable arity
(trigger* <and {?test-a ?test-b ?r}>
          (new (k)
               {[k {?test-a
                    ?test-b
                    (trigger {<pass> <pass>} <?r true>)
                    (trigger <fail> {<?r false>
                                     <kill>})}]
                (trigger <-kill> (trigger [k] nil))}))

;;; run all tests simultaneously. Possibly early-terminate any that do not
;;; need to be run
(trigger* <or (?test-a ?test-b ?r)>
          (new (k)
               {[k {?test-a
                    ?test-b
                    (trigger <pass> {<?r true>
                                     <kill>})
                    (trigger {<fail> <fail>} <?r false>)}]
                (trigger <-kill> (trigger [k] nil))}))
