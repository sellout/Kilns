#+xcvb (module (:depends-on ("package" "agents")))
(in-package :kell-calculus)

(defun closedp (agent)
  (null (free-variables agent)))

(defmacro define-set-relation (name (process1 process2) &body body)
  `(defgeneric ,name (,process1 ,process2)
     (:method ((,process1 process) (,process2 process))
       ,@body)
     (:method ((,process1 list) (,process2 list))
       "R^ = {␣S, S'␣ | ∀P ∈ S, ∃P' ∈ S', ␣P, P'␣ ∈ R}"
       (reduce #'every
               (mapcar (lambda (p)
                         (reduce #'some
                                 (mapcar (lambda (p*) (,name p p*))
                                         ,process2)))
                       ,process1)))))
