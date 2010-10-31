#+xcvb (module (:depends-on ("utilities" "syntax")))
(in-package :kell-calculus)

(defmethod structurally-congruent-p ((left pattern) (right pattern))
  (and (set= (free-names left) (free-names right))
       (set= (channel-names left) (channel-names right))
       (set= (union (bound-names left) (bound-variables left))
             (union (bound-names right) (bound-variables right)))))

(defgeneric sub-reduce (process)
  (:method ((process kell))
    (kell (name process)
          (let ((sub-process (sub-reduce (process process))))
            (if (typep sub-process 'restriction)
                (expand-restriction sub-process)
                sub-process))
          (continuation process)))
  (:method ((process parallel-composition))
    (map-process (lambda (proc)
                   (if (typep proc 'restriction)
                       (expand-restriction proc)
                       proc))
                 process)))

;; FIXME: move KILNS:EXECUTE-MATCH here and rename it to REDUCE
