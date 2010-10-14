(declaim (optimize (debug 3)))
(require :asdf)
(load "kilns.asd")
(asdf:load-system :kilns :verbose nil)

(defun application-toplevel ()
  (destructuring-bind (app &optional cpu-tag cpu-count kell)
                      (ccl::command-line-arguments)
    (declare (ignore app cpu-tag))
    (kilns::toplevel (if cpu-count (parse-integer cpu-count)) kell)))

(save-application "kilns"
                  :toplevel-function #'application-toplevel :prepend-kernel t)
