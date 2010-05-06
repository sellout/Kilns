(defpackage kilns-runner
  (:use #:cl #:kilns)
  (:shadow #:load #:read #:eval)
  (:export #:load #:read #:eval))

(defpackage kilns-user
  (:use #:kilns #:kilns-runner))

(in-package #:kilns-runner)

(defun eval (form)
  (cl:eval (list 'kilns::add-process form kilns::*top-kell*)))

(defun read (&rest read-args)
  (let (form)
    (let ((*read-eval* nil))
      (setf form (apply #'cl:read read-args)))
    (if *read-eval*
        (eval form)
        form)))

(defun load (file-name
             &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist :error))
  (let ((full-name (merge-pathnames file-name (make-pathname :type "kiln"))))
    (with-open-file (stream full-name :external-format :utf-8)
      (handler-case (loop until (read stream))
        (end-of-file () full-name)))))