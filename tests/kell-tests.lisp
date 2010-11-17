(in-package kilns-tests)

(def-suite kell-calculus
    :description "Tests for the kell calculus."
    :in kilns)

(in-suite kell-calculus)

(test should-assume-null-message-continuation
  (let ((process (message 'test (message 'test))))
    (is (match null (continuation process)))))

(test should-assume-null-message-argument
  (let ((process (message 'test)))
    (is (match null (argument process)))
    (is (match null (continuation process)))))

(test should-assume-null-kell-continuation
  (let ((process (kell 'test (message 'test))))
    (is (match null (continuation process)))))

(test should-compose-abstraction-and-process
  (let ((pattern-abstraction
         (make-instance 'pattern-abstraction
                        :pattern (kell-calculus::convert-process-to-pattern
                                  (message 'param (process-variable 'x)))
                        :process (process-variable 'x)))
        (process (message 'test)))
    (is (match (compose pattern-abstraction process)
               (make-instance 'application-abstraction
                              :abstraction pattern-abstraction
                              :concretion (make-instance
                                           'concretion
                                           :continuation process))))))

(test should-apply-abstraction
  (is (match (@ (make-instance 'pattern-abstraction
                               :pattern (kell-calculus::convert-process-to-pattern
                                         (message 'param (process-variable 'x)))
                               :process (process-variable 'x))
                (make-instance 'concretion
                               :messages (message 'param (message 'test))))
             (message 'test))))