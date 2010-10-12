(declaim (optimize (debug 3)))
(require :asdf)
(load "kilns.asd")
(asdf:load-system :kilns)

(defun application-toplevel ()
  (destructuring-bind (app &optional cpu-count kell)
                      (ccl::command-line-arguments)
    (declare (ignore app))
    (kilns::toplevel (parse-integer cpu-count) kell)))

(save-application "kilns"
                  :toplevel-function #'application-toplevel :prepend-kernel t)
