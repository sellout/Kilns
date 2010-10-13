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

;;; FIXME: for networking, load needs to be able to take a path to a subkell
;;;        that represents what is to be run in the local instance. It should
;;;        work as if the following (illegal) trigger were used:
;;;            (trigger [path [to [correct [kell ?process]]]] ?process)
(defun load (file-name
             &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist :error))
  (declare (ignore if-does-not-exist verbose))
  (let ((full-name (merge-pathnames file-name (make-pathname :type "kiln"))))
    (with-open-file (stream full-name :external-format :utf-8)
      (apply #'kilns::parallel-composition
             (reverse (loop for value = (read stream nil)
                        while value
                        do (if print (print value) value)
                        collecting value))))))

(defmacro lisp (&rest forms)
  `'(cl:progn ,@forms))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* kilns::*kilns-readtable*)
  (values))
