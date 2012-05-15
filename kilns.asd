(defpackage kilns-system
  (:use #:cl #:asdf))

(in-package #:kilns-system)

(defsystem kilns
  :version "0.0.4"
  :description "An implementation of the Kell calculus in a lisp-like language."
  :license "MIT"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (bordeaux-threads cl-unification iolib.os closer-mop
               iolib external-program quid-pro-quo) ; cl+ssl ironclad
  :pathname "src/"
  :components ((:file "package")
               (:module "kell-calculus"
                        :depends-on ("package")
                        :components ((:file "definitions"
                                            :depends-on ("labeled-transitions"
                                                         "processes"))
                                     (:file "identifiers"
                                            :depends-on ("labeled-transitions"))
                                     (:file "processes"
                                            :depends-on ("identifiers"))
                                     (:file "substitutions"
                                            :depends-on ("patterns"))
                                     (:file "patterns"
                                            :depends-on ("restrictions"))
                                     (:file "restrictions"
                                            :depends-on ("processes"))
                                     (:file "reduction-semantics"
                                            :depends-on ("processes"))
                                     (:file "labeled-transitions")
                                     (:file "commitment-relation"
                                            :depends-on ("processes"))
                                     (:file "congruences"
                                            :depends-on ("restrictions"))))
               (:module "pattern-languages"
                        :depends-on ("package" "kell-calculus")
                        :components ((:file "jk-calculus")
                                     (:file "pnpjk-calculus"
                                            :depends-on ("jk-calculus"))
                                     (:file "fraktal"
                                            :depends-on ("pnpjk-calculus"))))
               (:file "utilities" :depends-on ("package"))
               (:file "syntax" :depends-on ("package"))
               (:file "unification" :depends-on ("package" "kell-calculus"))
               (:file "reader"
                      :depends-on ("kell-calculus" "pattern-languages"))
               (:file "runtime"
                      :depends-on ("syntax" "reader" "unification"))
               (:file "network"
                      :depends-on ("runtime"))
               (:file "debug" :depends-on ("reader")))
  :in-order-to ((test-op (load-op kilns-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :kilns-tests)
                             (intern "KILNS" :kilns-tests))))

(defsystem kilns-tests
    :depends-on (kilns fiveam)
    :pathname "tests/src/"
    :components ((:file "package")
                 (:file "eval-tests" :depends-on ("package"))
                 (:file "reader-tests" :depends-on ("package"))
                 (:file "kell-tests" :depends-on ("package"))
                 ;; (:file "protocol-tests" :depends-on ("package"))
                 ))
