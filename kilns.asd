(defpackage kilns-system
  (:use #:cl #:asdf))

(in-package #:kilns-system)

(defsystem kilns
  :version "0.0.4"
  :description "An implementation of the Kell calculus in a lisp-like language."
  :license ""
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (cl-unification iolib.os closer-mop dispatch)
  :pathname "src/"
  :components ((:file "package")
               (:module "kell-calculus"
                        :depends-on ("package")
                        :components ((:file "identifiers"
                                            :depends-on ("labeled-transitions"))
                                     (:file "processes"
                                            :depends-on ("identifiers"))
                                     (:file "substitutions"
                                            :depends-on ("processes"))
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
               (:file "unification"
                      :depends-on ("package" "kell-calculus"))
               (:file "runtime"
                      :depends-on ("syntax" "kell-calculus" "unification"))
               (:file "reader" :depends-on ("runtime"))
               (:file "debug" :depends-on ("reader"))))
