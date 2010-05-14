(defpackage kilns-runner
  (:use #:cl #:kilns)
  (:shadow #:load #:read #:eval)
  (:export #:load #:read #:eval #:state #:lisp))

(defpackage kilns-user
  (:use #:kilns #:kilns-runner))

(in-package #:kilns-runner)

;; (defun eval (form)
;;   (kilns::add-process form kilns::*top-kell*))

(defun read (&rest read-args)
  (apply #'cl:read read-args))

(defun load (file-name
             &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist :error))
  (let ((full-name (merge-pathnames file-name (make-pathname :type "kiln"))))
    (with-open-file (stream full-name :external-format :utf-8)
      (reduce #'kilns::compose-processes
              (loop for value = (read stream nil)
                 while value
                 do (if print (print value) value)
                 collecting value)))))

(defmacro lisp (&rest forms)
  `'(cl:progn
      ,@forms
      null-process))
