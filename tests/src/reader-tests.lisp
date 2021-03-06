(in-package kilns-tests)

(def-suite reader
    :description "Tests for the Kilns reader."
    :in kilns)

(in-suite reader)

(test should-read-null-process
  (is (match null (eval (kilns::read-from-string "null")))))

(test should-read-empty-message
  (is (match (message 'test) (eval (kilns::read-from-string "{test}")))))

(test should-read-empty-kell
  (is (match (kell 'test null) (eval (kilns::read-from-string "[test null]")))))

#|
(test should-not-complain-about-unbound-variables
  (is (match (trigger (message 'load (process-variable 'filename))
                      `(load ,(process-variable 'filename)))
             (eval (kilns::read-from-string "(trigger {load ?filename} (load ?filename))")))))
|#
