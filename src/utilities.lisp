#+xcvb (module (:depends-on ("package")))
(in-package :kilns)

;;; Definitions for things assumed to exist by the Kell calculus

(defun set= (left right)
  (and (null (set-difference left right))
       (null (set-difference right left))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* *kilns-readtable*)
  (values))

(defmacro def ((name &rest parameters) &body body)
  "Allows us to define new operations. It's currently just like CL's DEFMACRO, but
   hopefully I can improve on that."
  `(defmacro ,name (,@parameters)
     ,@body))
