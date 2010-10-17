(in-package #:kilns-runner)

(defun state ()
  "Prints the current kell."
  ;; TODO: It would be great to pretty-print this, but just setting
  ;;       *PRINT-PRETTY* isn't enough.
  (print kilns::*top-kell*)
  null)
