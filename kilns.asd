(defpackage kilns-system
  (:use #:cl #:asdf))

(in-package #:kilns-system)

(defsystem kilns
  :version "0.0.3"
  :description "An implementation of the Kell calculus in a lisp-like language."
  :license ""
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (bordeaux-threads cl-unification)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "utilities" :depends-on ("package"))
                         (:file "agents" :depends-on ("package"))
                         (:file "processes" :depends-on ("package" "agents"))
                         (:file "syntax" :depends-on ("package"))
                         ;;(:file "identifier" :depends-on ("package" "processes"))
                         (:file "patterns" :depends-on ("package" "processes"))
                         (:file "unification"
                                :depends-on ("package" "processes" "patterns"))
                         ;;(:file "reduction-semantics" :depends-on ("utilities" "syntax"))
                         (:file "runtime"
                                :depends-on ("syntax" "processes" "patterns" "unification"))
                         (:file "reader" :depends-on ("runtime"))
                         (:file "debug" :depends-on ("reader"))
                         ;; pattern languages
                         (:file "jk-calculus" :depends-on ("patterns"))
                         (:file "pnpjk-calculus" :depends-on ("jk-calculus"))
                         (:file "fraktal" :depends-on ("pnpjk-calculus"))))))
