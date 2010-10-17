(declaim (optimize (debug 3)))

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
    (load quicklisp-init)
    (progn
      (load "build/quicklisp")
      (funcall (intern "INSTALL" :quicklisp-quickstart)))))

(ql:quickload "bordeaux-threads" :verbose nil)
(ql:quickload "cl-unification" :verbose nil)

(load "kilns.asd")
(asdf:load-system :kilns :verbose nil)

(defun application-toplevel ()
  (destructuring-bind (app &optional cpu-tag cpu-count kell)
                      (ccl::command-line-arguments)
    (declare (ignore app cpu-tag))
    (kilns::toplevel (if cpu-count (parse-integer cpu-count)) kell)))

(save-application "kilns"
                  :toplevel-function #'application-toplevel :prepend-kernel t)
