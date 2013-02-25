(in-package kilns-tests)

(def-suite reader
    :description "Tests for the Kilns reader."
    :in kilns)

(in-suite reader)

(test should-read-null-process
  (is (match +null-process+
             (eval (kilns::read-from-string "null")))))

(test should-read-empty-message
  (is (match (eval '(message kilns-user::test))
             (eval (kilns::read-from-string "{test}")))))

(test should-read-empty-kell
  (is (match (eval '(kell kilns-user::test null))
             (eval (kilns::read-from-string "[test null]")))))

#|
(test should-not-complain-about-unbound-variables
  (let ((*current-pattern-language* +jk-calculus+))
    (is (match (eval '(trigger (message load (process-variable filename))
                               (load filename)))
               (eval (kilns::read-from-string "(trigger {load ?filename}
                                                        (load filename))"))))))
|#
