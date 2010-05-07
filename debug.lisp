(in-package #:kilns-runner)

(defun state ()
  "Prints the current kell."
  (null (print kilns::*top-kell*)))
