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
