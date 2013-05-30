(in-package #:kilns-tests)

(def-suite eval
    :description "Testing the evaluator."
    :in kilns)

(in-suite eval)

(test should-evaluate-to-message
  (is (kilns::egal (make-instance 'message
                                  :name (make-instance 'global-name :label 'foo)
                                  :argument (compose (make-instance 'message
                                                                    :name (make-instance 'global-name :label 'bar))
                                                     (make-instance 'message
                                                                    :name (make-instance 'global-name :label 'baz))))
                   (eval '(message foo (par (message bar) (message baz)))))))

(test should-evaluate-to-kell
  (is (kilns::egal (make-instance 'kell
                            :name (make-instance 'global-name :label 'foo)
                            :state (compose (make-instance 'kell :name (make-instance 'global-name :label 'bar))
                                            (make-instance 'kell :name (make-instance 'global-name :label 'baz))))
             (eval '(kell foo (par (kell bar) (kell baz)))))))

(test should-evaluate-to-restriction
  (is (kilns::egal (let ((x (make-instance 'restricted-name :label 'x))
                         (y (make-instance 'restricted-name :label 'y)))
                     (make-instance 'restriction
                                    :names (list x y)
                                    :abstraction (compose (make-instance 'message
                                                                         :name x
                                                                         :argument (make-instance 'message
                                                                                                  :name y))
                                                          (make-instance 'message
                                                                         :name (make-instance 'global-name :label 'z)))))
                   (eval '(new (x y) (message x (message y)) (message z))))))

(test should-evaluate-to-jk-pattern
  (let ((*current-pattern-language* +jk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'foo)
                                                                     :argument (make-instance 'kilns::binding
                                                                                              :variable (make-instance 'process-variable
                                                                                                                       :label 'bar)))
                                                      (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'baz)
                                                                     :argument (make-instance 'kilns::binding
                                                                                              :variable (make-instance 'process-variable
                                                                                                                       :label 'zab))
                                                                     :continuation 'down))
                                    :process (compose (make-instance 'process-variable
                                                                     :label 'bar)
                                                      (make-instance 'process-variable
                                                                     :label 'zab)))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (process-variable zab))))
                                     bar zab))))))

(test pnpjk-should-allow-jk-patterns
  (let ((*current-pattern-language* +pnpjk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'foo)
                                                                     :argument (make-instance 'kilns::binding
                                                                                              :variable (make-instance 'process-variable
                                                                                                                       :label 'bar)))
                                                      (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'baz)
                                                                     :argument (make-instance 'kilns::binding
                                                                                              :variable (make-instance 'process-variable
                                                                                                                       :label 'zab))
                                                                     :continuation 'down))
                                    :process (compose (make-instance 'process-variable
                                                                     :label 'bar)
                                                      (make-instance 'process-variable
                                                                     :label 'zab)))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (process-variable zab))))
                                     bar zab))))))

(test should-evaluate-to-pnpjk-pattern
  (let ((*current-pattern-language* +pnpjk-calculus+))
    (is (kilns::egal (make-instance 'trigger
                                    :pattern (compose (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'foo)
                                                                     :argument (make-instance 'kilns::binding
                                                                                              :variable (make-instance 'process-variable
                                                                                                                       :label 'bar)))
                                                      (make-instance 'message
                                                                     :name (make-instance 'global-name :label 'baz)
                                                                     :argument (make-instance 'message
                                                                                              :name (make-instance 'kilns::binding
                                                                                                                   :variable (make-instance 'name-variable
                                                                                                                                            :label 'zab))
                                                                                              :argument +blank+)
                                                                     :continuation 'down))
                                    :process (compose (make-instance 'process-variable
                                                                     :label 'bar)
                                                      (make-instance 'message
                                                                     :name (make-instance 'name-variable
                                                                                          :label 'zab))))
                     (eval '(trigger (par (message foo (process-variable bar))
                                          (down (message baz
                                                         (message (name-variable zab)))))
                                     bar (message zab)))))))
