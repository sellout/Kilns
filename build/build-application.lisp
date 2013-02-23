(declaim (optimize (debug 3)))

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
    (load quicklisp-init)
    (progn
      (load "build/quicklisp")
      (funcall (intern "INSTALL" :quicklisp-quickstart)))))

(load "kilns.asd")
(ql:quickload "kilns" :verbose nil)
(ql:quickload "com.dvlsoft.clon" :verbose nil)

(com.dvlsoft.clon:defsynopsis (:postfix "FILES...")
  (text :contents "The Kilns programming language, based on the Kell calculus.")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit."))
  (lispobj :short-name "c" :long-name "cpu-count"
           :argument-name "NUMBER" :typespec '(integer 1)
           :description "The number of CPUs/cores available.")
  (stropt :short-name "k" :long-name "kell" :argument-name "NAME"
          :description "The name of the kell to use as the top kell.")
  (lispobj :short-name "p" :long-name "port"
           :argument-name "NUMBER" :typespec '(unsigned-byte 16)
           :description "The network port to listen for other kiln instances
                         on."))

(let ((files-to-load))
  (defmethod kilns:toplevel :before (&optional (top-kell kilns::*top-kell*))
    (mapcar (lambda (file)
              (kilns::add-process (kilns:eval `(kilns-user::load ,file))
                                  top-kell))
            files-to-load))

  (defun application-toplevel ()
    (com.dvlsoft.clon:make-context)
    (cond ((com.dvlsoft.clon:getopt :long-name "help")
           (com.dvlsoft.clon:help)
           (com.dvlsoft.clon:exit))
          ((com.dvlsoft.clon:getopt :long-name "version")
           (format t "kilns version ~a"
                   (asdf:component-version (asdf:find-system :kilns)))
           (com.dvlsoft.clon:exit)))
    (setf files-to-load (com.dvlsoft.clon:remainder))
    (kilns::start-event-source 20359)
    (kilns:run-toplevel
     :cpu-count (com.dvlsoft.clon:getopt :long-name "cpu-count")
     :local-kell (com.dvlsoft.clon:getopt :long-name "kell")
     :port-number (com.dvlsoft.clon:getopt :long-name "port"))))

;;; NOTE: Use the first of these lines to build a profilable library, and the
;;;       second to build an executable.
;; (ccl:save-application "kilns.dylib" :native t)
(com.dvlsoft.clon:dump "kilns" application-toplevel)
