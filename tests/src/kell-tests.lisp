(in-package #:kilns-tests)

(def-suite kell-calculus
    :description "Tests for the kell calculus."
    :in kilns)

(in-suite kell-calculus)

(test should-reduce-to-process
  (let ((*current-pattern-language* +jk-calculus+))
    (is (match (eval '(par (message yes) (message sir)))
          (@ (make-instance 'definition
                         :name 'test1
                         :pattern (kilns::define-pattern *current-pattern-language*
                                      (apply #'kilns::order-forms
                                             '((process-variable first)
                                               (process-variable second))))
                         :process (kilns::define-parallel-composition '(first second)))
             (make-instance 'named-concretion
                            :name 'test1
                            :messages (eval '(par (message 1 (message yes))
                                                  (message 2 (message sir))))))))))

(test should-not-reduce-to-process
  (let ((*current-pattern-language* +jk-calculus+))
    (is-false (match (eval '(par (message yes) (message sir)))
                (@ (eval '(define (test2 (process-variable first)
                                         (process-variable second))
                            (par first second)))
                   (make-instance 'named-concretion
                                  :name 'nope
                                  :messages (eval '(par (message 1 (message yes))
                                                        (message 2 (message sir))))))))))

(test should-not-reduce-to-process-with-pattern-abstraction
  (is-false (match (eval '(par (message yes) (message sir)))
              (@ (make-instance 'pattern-abstraction
                                :pattern 
                                (kilns::define-pattern +jk-calculus+
                                    '(par (message 1 (process-variable first))
                                          (message 2 (process-variable second))))
                                :process (eval '(par first second)))
                 (make-instance 'named-concretion
                                :name 'nope
                                :messages (eval '(par (message 1 (message yes))
                                                      (message 2 (message sir)))))))))

(test should-not-reduce-to-process-with-concretion
  (let ((*current-pattern-language* +jk-calculus+))
    (is-false (match (eval '(par (message yes) (message sir)))
                (@ (eval '(define (test3 (process-variable first)
                                   (process-variable second))
                            first second))
                   (make-instance 'concretion
                                  :messages (eval '(par (message 1 (message yes))
                                                        (message 2 (message sir))))))))))

(test should-assume-null-message-continuation
  (let ((process (eval '(message test (message test)))))
    (is (match +null-process+ (continuation process)))))

(test should-assume-nil-message-argument-and-null-continuation
  (let ((process (eval '(message test))))
    (is (match +null-process+ (argument process)))
    (is (match +null-process+ (continuation process)))))

(test should-assume-null-kell-continuation
  (let ((process (eval '(kell test (message test)))))
    (is (match +null-process+ (continuation process)))))

(test should-compose-abstraction-and-process
  (let* ((pattern-abstraction
          (make-instance 'pattern-abstraction
                         :pattern (kilns::define-pattern +jk-calculus+
                                      '(message param (process-variable x)))
                         :process (eval 'x)))
         (process (eval '(message test)))
         (result (make-instance 'application-abstraction
                                :abstraction pattern-abstraction
                                :concretion (make-instance
                                             'concretion
                                             :continuation process))))
    (is (match (compose pattern-abstraction process) result))
    (is (match (compose process pattern-abstraction) result))))

(test should-compose-concretion-and-process
  (let* ((concretion-process (eval '(message concreted)))
         (concretion (make-instance 'concretion
                                    :restricted-names '(x)
                                    :messages (eval '(message matching))
                                    :continuation concretion-process))
         (process (eval '(message test)))
         (result (make-instance 'concretion
                                :restricted-names '(x)
                                :messages (eval '(message matching))
                                :continuation (compose concretion-process
                                                       process))))
    (is (match (compose concretion process) result))
    (is (match (compose process concretion) result))))

(test should-compose-concretions
  (let ((a (make-instance 'concretion
                          :restricted-names '(x)
                          :messages (eval '(message matching))
                          :continuation (eval '(message que))))
        (b (make-instance 'concretion
                          :restricted-names '(y)
                          :messages (eval '(message match2))
                          :continuation (eval '(message pasa)))))
    (is (match (make-instance 'concretion
                              :restricted-names '(x y)
                              :messages (eval '(par (message matching)
                                                    (message match2)))
                              :continuation (eval '(par (message que)
                                                        (message pasa))))
               (compose a b)))))

(test should-apply-abstraction
  (is (match (eval '(message test))
             (@ (make-instance 'pattern-abstraction
                               :pattern (kilns::define-pattern +jk-calculus+
                                            '(message param (process-variable x)))
                               :process (eval 'x))
                (make-instance 'concretion
                               :messages (eval '(message param
                                                         (message test))))))))

(test should-suspend-application
  (let ((pattern-abstraction
         (make-instance 'pattern-abstraction
                        :pattern (kilns::define-pattern +jk-calculus+
                                            '(message param (process-variable x)))
                        :process (eval 'x)))
        (concretion
         (make-instance 'concretion
                        :messages (eval '(message not-param (message test))))))
    (is (match (@ pattern-abstraction concretion)
               (make-instance 'application-abstraction
                              :abstraction pattern-abstraction
                              :concretion concretion)))))
