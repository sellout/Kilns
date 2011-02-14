(in-package #:kilns)

;; (defun eval (form)
;;   (add-process form *top-kell*))

(defun read (&rest read-args)
  (let* ((*readtable* kilns::*kilns-readtable*)
         (*package* (find-package :kilns-user))
         (value (apply #'cl:read read-args)))
    (case value
      (null null)
      (otherwise value))))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace
                                &aux idx)
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (values
   (with-input-from-string (stream string :index idx :start start :end end)
     (if preserve-whitespace
       (read-preserving-whitespace stream eof-error-p eof-value)
       (read stream eof-error-p eof-value)))
   idx))

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
      (apply #'parallel-composition
             (reverse (loop for value = (read stream nil)
                        while value
                        do (if print (print value) value)
                        collecting value))))))

(defmacro lisp (&rest forms)
  `'(cl:progn ,@forms))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* *kilns-readtable*)
  (values))
