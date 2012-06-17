#+xcvb (module (:depends-on ("utilities" "syntax")))
(in-package :kell-calculus)

(defun unique-name (name)
  "Returns a new globally (across threads) unique name."
  (gensym (format nil "~a-~d-" name (ccl::process-tcr (current-thread)))))

(defgeneric apply-restriction (local-name global-name process)
  (:documentation "Replaces all instances of a restricted name with a globally-
                   unique name.")
  (:method (local-name global-name process)
    (declare (ignore local-name global-name))
    process)
  (:method (local-name global-name (process name))
    (if (eq process local-name)
        (values global-name t)
        (values process nil)))
  (:method (local-name global-name (process message))
    (make-instance 'message
                   :name (apply-restriction local-name
                                            global-name
                                            (name process))
                   :argument (apply-restriction local-name
                                                global-name
                                                (argument process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))))
  (:method (local-name global-name (process kell))
    (make-instance 'kell
                   :name (apply-restriction local-name
                                            global-name
                                            (name process))
                   :state (apply-restriction local-name
                                             global-name
                                             (state process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))))
  (:method (local-name global-name (process parallel-composition))
    (map-process (lambda (proc) (apply-restriction local-name global-name proc))
                 process))
  (:method (local-name global-name (process pattern))
    (flet ((apply-res (pattern)
             (mapcar (lambda (message)
                       (apply-restriction local-name global-name message))
                     pattern)))
      (make-instance 'pattern
                     :local-message-pattern
                     (apply-res (local-message-pattern process))
                     :down-message-pattern
                     (apply-res (down-message-pattern process))
                     :up-message-pattern
                     (apply-res (up-message-pattern process))
                     :kell-message-pattern
                     (apply-res (kell-message-pattern process))
                     :named-concretions
                     (apply-res (named-concretions process))
                     :placeholders
                     (apply-res (placeholders process)))))
  (:method (local-name global-name (process restriction-abstraction))
    (make-instance (class-of process)
                   :names (names process)
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))))
  (:method (local-name global-name (process pattern-abstraction))
    (make-instance (class-of process)
                   :pattern (apply-restriction local-name
                                               global-name
                                               (pattern process))
                   :process (apply-restriction local-name
                                               global-name
                                               (process process))))
  (:method (local-name global-name (process kell-abstraction))
    (make-instance (class-of process)
                   :name (apply-restriction local-name
                                            global-name
                                            (name process))
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))))
  (:method (local-name global-name (process application-abstraction))
    (make-instance (class-of process)
                   :abstraction (apply-restriction local-name
                                                   global-name
                                                   (abstraction process))
                   :concretion (apply-restriction local-name
                                                  global-name
                                                  (concretion process))))
  (:method (local-name global-name (process concretion))
    (make-instance (class-of process)
                   :restricted-names (restricted-names process)
                   :messages (apply-restriction local-name
                                                global-name
                                                (messages process))
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))))
  (:method (local-name global-name (process named-concretion))
    (make-instance (class-of process)
                   :name (name process)
                   :restricted-names (restricted-names process)
                   :messages (messages process)
                   :continuation (apply-restriction local-name
                                                    global-name
                                                    (continuation process))
                   :lexical-names (mapcar (alexandria:curry #'cl:substitute global-name local-name)
                                          (lexical-names process))
                   :lexical-placeholders (lexical-placeholders process)
                   :suspended-values (suspended-values process))))

(defgeneric sub-reduce (process)
  (:documentation "The sub-reduction relation is defined to handle scope
                   extrusion of restriction out of kell boundaries.")
  (:method-combination contract)
  (:method :guarantee "process hasn't changed if there was no reduction"
           (process)
    (multiple-value-bind (new-process reducedp) (results)
      (not (eq reducedp (eq process new-process)))))
  (:method (process)
    (values process nil))
  (:method ((process kell))
    (multiple-value-bind (reduced-state reducedp) (sub-reduce (state process))
      (values (if reducedp
                  (make-instance 'kell
                                 :name (name process)
                                 :state reduced-state
                                 :continuation (continuation process))
                  process)
              reducedp)))
  (:method ((process restriction-abstraction))
    (let ((abstraction (abstraction process)))
      (mapc (lambda (name)
              (setf abstraction
                    ;; TODO: apply-restriction should handle all names
                    ;;       at once, rather than one at a time
                    (apply-restriction name (extrude-scope name) abstraction)))
            (names process))
      (values abstraction t)))
  (:method ((process concretion))
    (if (length (restricted-names process))
        (values (let ((messages (messages process))
                      (continuation (continuation process)))
                  (mapc (lambda (name)
                          (let ((global-name (extrude-scope name)))
                            (psetf messages
                                   (apply-restriction name global-name messages)
                                   continuation
                                   (apply-restriction name
                                                      global-name
                                                      continuation))))
                        (restricted-names process))
                  (make-instance (class-of process)
                                 :messages messages :continuation continuation))
                t)
        (values process nil)))
  (:method ((process parallel-composition))
    (let* ((reducedp nil)
           (new-processes
            (map-parallel-composition (lambda (proc)
                                        (multiple-value-bind (reduced-proc redp)
                                            (sub-reduce proc)
                                          (when redp
                                            (setf reducedp t))
                                          reduced-proc))
                                      process)))
      (values (if reducedp
                  (apply #'parallel-composition new-processes)
                  process)
              reducedp))))

#|
(defun delta (local-messages)
  (values (mapcar (lambda (message) (message (name message) (argument message)))
                  local-messages)
          (reduce #'compose (mapcar #'continuation local-messages))))

(defun upsilon (kells)
  (values (mapcar (lambda (kell) (kell (name kell) (state kell))) kells)
          (reduce #'compose (mapcar #'continuation kells))))

(defun psi (sub-kells)
  (values (mapcan (lambda (kell) (delta (messages kell))) sub-kells)
   (reduce #'compose (mapcar #'))))
|#

;; FIXME: move KILNS:EXECUTE-MATCH here and rename it to REDUCE
