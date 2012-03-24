#+xcvb (module (:depends-on ("package")))
(in-package :kilns)

;;; Definitions for things assumed to exist by the Kell calculus

(defun set= (left right)
  (null (set-exclusive-or left right)))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))
