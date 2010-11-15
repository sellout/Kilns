(in-package kilns-tests)

(def-suite protocol
    :description "Tests for the network protocol."
    :in kilns)

(in-suite protocol)

(def-fixture kilns-socket ()
  ;; FIXME: need to pull startup code out of TOPLEVEL
  (let ((socket (kilns::kilns-listener)))
    (unwind-protect (&body)
      (when socket (close socket)))))

(test should-open-socket
  (with-fixture kilns-socket ()
    (is-true socket "START-KILNS did not open a socket for listening")))
