(in-package #:kilns-tests)

(def-suite eval
    :description "Testing the evaluator."
    :in kilns)

(in-suite eval)

(test should-evaluate-to-message
  (is (kilns::egal (make-instance 'message
                            :name 'foo
                            :argument (compose (make-instance 'message
                                                              :name 'bar)
                                               (make-instance 'message
                                                              :name 'baz)))
             (eval '(message foo (par (message bar) (message baz)))))))

(test should-evaluate-to-kell
  (is (kilns::egal (make-instance 'kell
                            :name 'foo
                            :state (compose (make-instance 'kell :name 'bar)
                                            (make-instance 'kell :name 'baz)))
             (eval '(kell foo (par (kell bar) (kell baz)))))))

(test should-evaluate-to-restriction
  (is (kilns::egal (make-instance 'restriction
                            :names '(x y)
                            :abstraction (compose (make-instance 'message
                                                                 :name 'x
                                                                 :argument (make-instance 'message
                                                                                          :name 'y))
                                                  (make-instance 'message
                                                                 :name 'z)))
                   (eval '(new (x y) (message x (message y)) (message z))))))

(test should-evaluate-to-jk-pattern
  (let ((*current-pattern-language* +jk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name 'foo
                                                                     :argument (make-instance 'process-variable :name 'bar))
                                                      (make-instance 'message
                                                                     :name 'baz
                                                                     :argument (make-instance 'process-variable :name 'zab)
                                                                     :continuation 'down))
                                    :process (compose 'bar 'zab))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (process-variable zab))))
                                     bar zab))))))

(test pnpjk-should-allow-jk-patterns
  (let ((*current-pattern-language* +pnpjk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name 'foo
                                                                     :argument (make-instance 'process-variable :name 'bar))
                                                      (make-instance 'message
                                                                     :name 'baz
                                                                     :argument (make-instance 'process-variable :name 'zab)
                                                                     :continuation 'down))
                                    :process (compose 'bar 'zab))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (process-variable zab))))
                                     bar zab))))))

(test should-evaluate-to-pnpjk-pattern
  (let ((*current-pattern-language* +pnpjk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name 'foo
                                                                     :argument (make-instance 'process-variable :name 'bar))
                                                      (make-instance 'message
                                                                     :name 'baz
                                                                     :argument (make-instance 'message :name (make-instance 'name-variable :name 'zab) :argument +blank+)
                                                                     :continuation 'down))
                                    :process (compose 'bar
                                                      (make-instance 'message
                                                                     :name 'zab)))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (message (name-variable zab)))))
                                     bar (message zab)))))))
