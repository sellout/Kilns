(in-package #:kilns-runner)

(defun system-state ()
  "Prints the current kell."
  ;; TODO: It would be great to pretty-print this, but just setting
  ;;       *PRINT-PRETTY* isn't enough.
  (print kilns::*top-kell*)
  null)

(defun sort-triggers (kell)
  (let (uplocal uplocaldown updown localdown up local down)
    (mapc (lambda (trigger)
            (let* ((pattern (pattern trigger)))
              (if (up-message-pattern pattern)
                  (if (or (local-message-pattern pattern)
                          (kell-message-pattern pattern))
                      (if (down-message-pattern pattern)
                          (push pattern uplocaldown)
                          (push pattern uplocal))
                      (if (down-message-pattern pattern)
                          (push pattern updown)
                          (push pattern up)))
                  (if (or (local-message-pattern pattern)
                          (kell-message-pattern pattern))
                      (if (down-message-pattern pattern)
                          (push pattern localdown)
                          (push pattern local))
                      (if (down-message-pattern pattern)
                          (push pattern down)
                          )))))
          (kell-calculus::triggers-in (state kell)))
    (list uplocal updown uplocaldown localdown up local down)))

(defun kell-dot (kell stream)
  (format stream "\"~w\" [label=\"~w\"];~%" (sxhash (princ-to-string kell)) (name kell))
  (mapc (lambda (subkell)
          (kell-dot subkell stream)
          (format stream "\"~w\" -> \"~w\";~%"
                  (sxhash (princ-to-string subkell))
                  (sxhash (princ-to-string kell))))
        (subkells kell)))

#| FIXME: this one dumps the kell contents, but it don't work yet

(defun kell-dot (kell stream)
  (let ((triggers (sort-triggers kell)))
    (format stream "\"~w\" [label=\"{~{~w~^\\n~}|{~{~w~^\\n~}|~{~w~^\\n~}}|~{~w~^\\n~}}|~{~w~^\\n~}|{~{~w~^\\n~}|~{~w~^\\n~}|~{~w~^\\n~}}\"];~%"
            (name kell)
            (first triggers)
            (second triggers)
            (third triggers)
            (fourth triggers)
            (kell-calculus::messages-in (state kell))
            (fifth triggers)
            (sixth triggers)
            (seventh triggers))
    (mapc (lambda (subkell)
            (kell-dot subkell stream)
            (format stream "\"~w\" -> \"~w\";~%" (name subkell) (name kell)))
          (subkells kell))))
|#

(defun dump-system-state (pathname)
  (with-open-file (dot (merge-pathnames pathname (make-pathname :type "dot"))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format dot "digraph KilnState {
  dpi=70;
  compound=true;
  clusterrank=local;
  edge [fontsize=8];
  node [shape=record, fontsize=10];~%")
    (kell-dot kilns::*top-kell* dot)
    (format dot "}~%")))
