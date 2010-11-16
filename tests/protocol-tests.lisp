(in-package kilns-tests)

(def-suite protocol
    :description "Tests for the network protocol."
    :in kilns)

(in-suite protocol)

(def-fixture kilns-socket ()
  ;; FIXME: need to pull startup code out of TOPLEVEL
  (let ((server (kilns::start-kilns-listener)))
    (unwind-protect (&body)
      (when server
        (bordeaux-threads:destroy-thread server)))))

(test should-open-socket
  (with-fixture kilns-socket ()
    (is-true server
             "START-KILNS did not open a socket for listening")))

(test should-accept-keepalive
  (with-fixture kilns-socket ()
    (sockets:with-open-socket
        (client :connect :active :address-family :internet :type :stream
                :external-format '(:utf-8 :eol-style :lf) :ipv6 nil
                :remote-host (sockets:lookup-hostname "localhost")
                :remote-port kilns::*base-port*)
      (let ((*readtable* kilns::*kilns-readtable*)
            (*package* (find-package :kilns-user)))
        (write (eval (read-from-string "{keep-alive}")) :stream client)
        (finish-output client)
        (is (match-local (eval (read-from-string "{keep-alive}"))
                         (eval (read client))))))))

(test should-handshake
  (with-fixture kilns-socket ()
    (sockets:with-open-socket
        (client :connect :active :address-family :internet :type :stream
                :external-format '(:utf-8 :eol-style :lf) :ipv6 nil
                :remote-host (sockets:lookup-hostname "localhost")
                :remote-port kilns::*base-port*)
      (let ((*readtable* kilns::*kilns-readtable*)
            (*package* (find-package :kilns-user)))
        (write (eval (read-from-string "{handshake (par {path {path {to {local {kell}}}}} {version \"0.0.1\"})}"))
               :stream client)
        (finish-output client)
        (is (match-local (eval (read-from-string "{handshake (par {path {path {to {local {kell}}}}} {version \"0.0.1\"})}"))
                         (eval (read client))))))))

(test should-reject-incompatible-protocol-version
  (with-fixture kilns-socket ()
    (sockets:with-open-socket
        (client :connect :active :address-family :internet :type :stream
                :external-format '(:utf-8 :eol-style :lf) :ipv6 nil
                :remote-host (sockets:lookup-hostname "localhost")
                :remote-port kilns::*base-port*)
      (let ((*readtable* kilns::*kilns-readtable*)
            (*package* (find-package :kilns-user)))
        (write (eval (read-from-string "{handshake (par {path {path {to {local {kell}}}}} {version \"10000.0.0\"})}"))
               :stream client)
        (finish-output client)
        (is (match-local (eval (read-from-string "{handshake {error ?message}}"))
                         (eval (read client))))))))
