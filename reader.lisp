(defpackage kiln-runner
  (:use #:cl #:kilns)
  (:shadow #:load #:read #:eval)
  (:export #:load #:read #:eval))

(defpackage kiln-user
  (:use #:kilns))

(in-package #:kiln-runner)

(defvar *top-kell* (make-instance 'kell))

(eval-when (:load-toplevel)
  (start-kiln))

(defun eval (form)
  (cl:eval (list 'kilns::add-process *top-kell* form)))

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