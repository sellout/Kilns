#+xcvb (module (:depends-on ("package")))
(in-package :kilns)

;;; Definitions for things assumed to exist by the Kell calculus

(defun set= (left right)
  (and (null (set-difference left right))
       (null (set-difference right left))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))
