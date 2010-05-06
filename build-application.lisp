(require :asdf)
(load "kilns.asd")
(asdf:load-system :kilns)
(save-application "kilns" :toplevel-function #'kilns::toplevel :prepend-kernel t)
