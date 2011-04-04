(in-package kilns-tests)

(def-suite kell-calculus
    :description "Tests for the kell calculus."
    :in kilns)

(in-suite kell-calculus)

(test should-assume-null-message-continuation
  (let ((process (message 'test (message 'test))))
    (is (match null (continuation process)))))

(test should-assume-nil-message-argument-and-null-continuation
  (let ((process (message 'test)))
    (is (match nil (argument process)))
    (is (match null (continuation process)))))

(test should-assume-null-kell-continuation
  (let ((process (kell 'test (message 'test))))
    (is (match null (continuation process)))))

(test should-compose-abstraction-and-process
  (let* ((pattern-abstraction
         (make-instance 'pattern-abstraction
                        :pattern (kell-calculus::convert-process-to-pattern
                                  (message 'param (process-variable 'x)))
                        :process (process-variable 'x)))
        (process (message 'test))
        (result (make-instance 'application-abstraction
                               :abstraction pattern-abstraction
                               :concretion (make-instance
                                            'concretion
                                            :continuation process))))
    (is (match (compose pattern-abstraction process) result))
    (is (match (compose process pattern-abstraction) result))))

(test should-compose-concretion-and-process
  (let* ((concretion-process (message 'concreted))
         (concretion (make-instance 'concretion
                                    :restricted-names '(x)
                                    :messages (message 'matching)
                                    :continuation concretion-process))
         (process (message 'test))
         (result (make-instance 'concretion
                                :restricted-names '(x)
                                :messages (message 'matching)
                                :continuation (compose concretion-process
                                                       process))))
    (is (match (compose concretion process) result))
    (is (match (compose process concretion) result))))

(test should-compose-concretions
  (let ((a (make-instance 'concretion
                          :restricted-names '(x)
                          :messages (list (message 'matching))
                          :continuation (message 'que)))
        (b (make-instance 'concretion
                          :restricted-names '(y)
                          :messages (list (message 'match2))
                          :continuation (message 'pasa))))
    (is (match (compose a b)
               (make-instance 'concretion
                              :restricted-names '(x y)
                              :messages (list (message 'matching)
                                              (message 'match2))
                              :continuation (par (message 'que)
                                                 (message 'pasa)))))))

(test should-apply-abstraction
  (is (match (@ (make-instance 'pattern-abstraction
                               :pattern (kell-calculus::convert-process-to-pattern
                                         (message 'param (process-variable 'x)))
                               :process (process-variable 'x))
                (make-instance 'concretion
                               :messages (message 'param (message 'test))))
        (message 'test))))

(test should-suspend-application
  (let ((pattern-abstraction
         (make-instance 'pattern-abstraction
                        :pattern (kell-calculus::convert-process-to-pattern
                                  (message 'param (process-variable 'x)))
                        :process (process-variable 'x)))
        (concretion
         (make-instance 'concretion
                        :messages (list (message 'not-param (message 'test))))))
    (is (match (@ pattern-abstraction concretion)
               (make-instance 'application-abstraction
                              :abstraction pattern-abstraction
                              :concretion concretion)))))
