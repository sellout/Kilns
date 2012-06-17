(in-package #:kilns)

;;#.(ql:quickload "hunchentoot")
;;#.(ql:quickload "cl-json")

(defun start-event-source (&optional (port 8080))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port port
                                 :access-log-destination nil)))
    (setf (hunchentoot:acceptor-document-root acceptor)
          (truename (asdf:system-relative-pathname :kilns "docroot")))
    (hunchentoot:start acceptor))
  (hunchentoot:define-easy-handler (get-kells :uri "/current-kells") ()
    (setf (hunchentoot:content-type*) "application/json")
    (kell-svg)))

(defun kell-svg ()
  (with-output-to-string (json)
    (json:encode-json (collect-kell-info *top-kell*) json)))

(defun unique-label (obj)
  (let ((name (name obj)))
    (format nil "~A~:[~;(~A.~A)~]"
            (label name) (or (thread name) (id name)) (thread name) (id name))))

(defun unique-id (obj)
  (format nil "~@[~A/~]~A"
          (and (slot-boundp obj 'parent) (unique-id (parent obj)))
          (unique-label obj)))

(defun collect-kell-info (kell)
  (list (cons :id (unique-id kell))
        (cons :name (unique-label kell))
        (cons :children (mapcar #'collect-kell-info (kell-calculus::kells-in (state kell))))
        (cons :data (list (cons :size (length (append (kell-calculus::process-variables-in (state kell))
                                                      (kell-calculus::messages-in (state kell))
                                                      ;; (kell-calculus::kells-in (state kell))
                                                      (kell-calculus::triggers-in (state kell))
                                                      (kell-calculus::named-concretions-in (state kell))
                                                      (kell-calculus::primitives-in (state kell)))))
                          (cons :active (kell-calculus::activep kell))
                          (cons :repl (zerop (random 10)))))))
