(defpackage kilns-system
  (:use #:cl #:asdf))

(in-package #:kilns-system)

(defsystem kilns
  :description "An implementation of the Kell calculus in a lisp-like language."
  :licence ""
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (bordeaux-threads cl-unification)
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:file "processes" :depends-on ("package"))
               ;;(:file "syntax" :depends-on ("package"))
               ;;(:file "identifier" :depends-on ("package" "processes"))
               (:file "patterns" :depends-on ("package" "processes"))
               (:file "unification" :depends-on ("package" "processes" "patterns"))
               ;;(:file "reduction-semantics" :depends-on ("utilities" "syntax"))
               (:file "runtime" :depends-on ("processes" "patterns" "unification"))
               (:file "reader" :depends-on ("runtime"))
               (:file "jk-calculus" :depends-on ("patterns"))
               ;;(:file "pnpjk-calculus" :depends-on ("syntax"))
               ;;(:file "fraktal" :depends-on ("syntax"))
               ))
