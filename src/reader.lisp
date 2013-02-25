(in-package #:kilns)

(defclass global-name (name)
  ((id :initform nil :initarg :id :reader id :type (or null integer))
   (thread :initform nil :initarg :thread :reader thread :type (or null integer))
   (host :initform nil :initarg :host :reader host :type (or null))))

(defmethod print-object ((obj global-name) stream)
  (if *debugp*
      (format stream "«~S~@[-~D~]~@[/~D~]»" (label obj) (thread obj) (id obj))
      (format stream "~S" (label obj))))

(defmethod unify
    ((pattern global-name) (agent global-name)
     &optional (substitutions (make-empty-environment)) &key &allow-other-keys)
  (if (or (eq pattern agent)
          (and (eql (label pattern) (label agent))
               (eql (id pattern) (id agent))
               (eql (thread pattern) (thread agent))
               (eql (host pattern) (host agent))))
      substitutions
      (error 'unification-failure
             :format-control "Could not unify the name ~A with the name ~A"
             :format-arguments (list pattern agent))))

(defclass restricted-name (name)
  ())

(defmethod print-object ((obj restricted-name) stream)
  (format stream (if *debugp* "⟦~S⟧" "~S") (label obj)))

(defmethod unify
    ((pattern restricted-name) (agent restricted-name)
     &optional (substitutions (make-empty-environment)) &key &allow-other-keys)
  (if (eq pattern agent)
      substitutions
      (error 'unification-failure
             :format-control "Could not unify the name ~A with the name ~A"
             :format-arguments (list pattern agent))))

(defvar *id-counter* 0) ; thread-local

(defmethod extrude-scope ((name restricted-name))
  "Converts a restricted name into a globally-unique name."
  (make-instance 'global-name
                 :label (label name)
                 :id (setf *id-counter* (1+ *id-counter*))
                 :thread (ccl::process-tcr (current-thread))))

(defun set-egal (left right)
  (null (set-exclusive-or left right :test #'egal)))

(defgeneric egal (x y)
  (:documentation "Henry Baker's EGAL, applied to Kilns.")
  (:method (x y)
    (eq x y))
  (:method ((x symbol) (y symbol))
    (equal (symbol-name x) (symbol-name y)))
  (:method ((x global-name) (y global-name))
    (and (egal (label x) (label y))
         (egal (id x) (id y))
         (egal (thread x) (thread y))
         (egal (host x) (host y))))
  (:method ((x message) (y message))
    (and (egal (name x) (name y))
         (egal (argument x) (argument y))
         (egal (continuation x) (continuation y))))
  (:method ((x kell) (y kell))
    (and (egal (name x) (name y))
         (egal (state x) (state y))
         (egal (continuation x) (continuation y))))
  (:method ((x parallel-composition) (y parallel-composition))
    (set-egal (kell-calculus::processes-in x)
              (kell-calculus::processes-in y)))
  (:method ((x restriction) (y restriction))
    (and (set-egal (names x) (names y))
         (egal (abstraction x) (abstraction y))))
  (:method ((x trigger) (y trigger))
    (and (egal (pattern x) (pattern y))
         (egal (process x) (process y))))
  (:method ((x pattern) (y pattern))
    (and (set-egal (local-message-pattern x) (local-message-pattern y))
         (set-egal (down-message-pattern x) (down-message-pattern y))
         (set-egal (up-message-pattern x) (up-message-pattern y))
         (set-egal (kell-message-pattern x) (kell-message-pattern y)))))

(defvar *global-names* ()
  "This only holds truly global names, not ones that have been extruded from
   restricted names.")

(defvar *restricted-names* ()
  "This is a list of lists of restricted names. Each list represents one scoping
   level.")

(defvar *process-variables* ()
  "This is a list of lists of process-variables. Each list represents one
   scoping level.")

(defun find-name (label restricted-names)
  (loop for scope in restricted-names
     for name = (find label scope :key #'label)
     until name
     finally (return name)))

(defun find-or-add-global-name (label)
  (or (find label *global-names* :key #'label)
      (let ((new-name (make-instance 'global-name :label label)))
        (push new-name *global-names*)
        new-name)))

(defun find-process-variable (label process-variables)
  (or (loop for scope in process-variables
         for pv = (find label scope :key #'label)
         until pv
         finally (return pv))
      (if kell-calculus::*force-resolution-p*
          label
          (error 'no-such-variable-error :name label))))

(defvar *reading-name-p* nil)
(defvar *reading-concretion-p* nil)

(defgeneric define-pattern (pattern-language pattern)
  (:documentation "This needs to be defined for each pattern language. It
                   determines how the patterns are interpreted.")
  (:method-combination contract)
  (:method :guarantee (pattern-language pattern)
     (declare (ignore pattern-language pattern))
     (= 3 (length (multiple-value-list (results)))))
  (:method :around (pattern-language pattern)
     ;; FIXME: this is a workaround since define-pattern-* currently aren’t
     ;;        collecting their variables for us.
     (let* ((proc (call-next-method))
            (pat (kell-calculus::convert-process-to-pattern proc)))
       (values proc
               (bound-names pattern-language pat)
               (bound-variables pattern-language pat)))))

(defun define-name (label)
  "Returns either a name object or a bare symbol. The symbol indicates "
  (or (find-name label *restricted-names*)
      (if *reading-concretion-p*
          label
          (find-or-add-global-name label))))

(defun define-placeholder (label)
  (if *reading-concretion-p*
      label
      (find-process-variable label *process-variables*)))

(defun define-kell
    (name &rest state)
  (make-instance 'kell
                 :name (let ((*reading-name-p* t)) (eval name))
                 :state (if state
                            (define-parallel-composition state)
                            +null-process+)))

(defun define-message (name &rest argument)
  (make-instance 'message
                 :name (let ((*reading-name-p* t)) (eval name))
                 :argument (if argument
                               (define-parallel-composition argument)
                               +null-process+)))

(defun define-parallel-composition (processes)
  (apply #'parallel-composition (mapcar #'eval processes)))

(defun define-restriction (names &rest processes)
  (let ((names (mapcar (alexandria:curry #'make-instance 'restricted-name :label)
                       (if (listp names) names (list names)))))
    (make-instance 'restriction
                   :names names
                   :abstraction (let ((*restricted-names* (cons names *restricted-names*)))
                                  (define-parallel-composition processes)))))

(defun define-trigger (pattern &rest processes)
  (multiple-value-bind (pattern name-variables process-variables)
      (define-pattern *current-pattern-language* pattern)
    (make-instance 'trigger
                   :pattern pattern
                   :process (let ((*restricted-names* (cons name-variables *restricted-names*))
                                  (*process-variables* (cons process-variables *process-variables*)))
                              (define-parallel-composition processes)))))

(defun order-forms (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  (let ((index 0))
    (cons 'par
          (mapcar (lambda (process)
                    `(message ,(incf index) ,process))
                  processes))))

(defgeneric count-concretions (process)
  (:documentation "This allows us to decide whether a particular object is a
                   process or a pattern.")
  (:method (process)
    (declare (ignore process))
    0)
  (:method ((process concretion))
    (1+ (count-concretions (messages process))))
  (:method ((process message))
    (+ (count-concretions (argument process))
       (count-concretions (continuation process))))
  (:method ((process kell))
    (+ (count-concretions (state process))
       (count-concretions (continuation process))))
  (:method ((process parallel-composition))
    (reduce #'+ (map-parallel-composition #'count-concretions process)))
  (:method ((process trigger))
    "If it has a trigger, it must be a process."
    0)
  (:method ((process restriction))
    "If it has a restriction, it must be a process."
    0)
  (:method ((process pattern))
    (+ (count-concretions (local-message-pattern process))
       (count-concretions (up-message-pattern process))
       (count-concretions (down-message-pattern process))
       (count-concretions (kell-message-pattern process))
       (count-concretions (named-concretions process)))))

(defun order-procs (&rest processes)
  "Works like the `,` sequencer defined in the paper, making parallel processes
   that are named by sequential integers."
  ;; TODO: This currently tries to parse as both a process and a pattern, and
  ;;      (if both succeed) it chooses the one with fewer concretions (since
  ;;       various special forms would be read as concretions in the wrong
  ;;       context).
  (let ((index 0))
    (apply #'parallel-composition
           (mapcar (lambda (process)
                     (let ((new-proc (ignore-errors (eval process)))
                           (new-patt (ignore-errors (define-pattern *current-pattern-language*
                                                                    process)))
                           (new-patt-component (ignore-errors (define-pattern-message-argument *current-pattern-language*
                                                                                               process))))
                       (make-instance 'message
                                      :name (find-or-add-global-name (incf index))
                                      :argument (car (stable-sort (remove nil
                                                                          (list new-proc new-patt-component new-patt))
                                                                  #'<
                                                                  :key #'count-concretions)))))
                   processes))))

(defun define-definition (pattern &rest processes)
  (destructuring-bind (name &rest parameters) pattern
    (multiple-value-bind (pattern name-variables process-variables)
        (define-pattern *current-pattern-language*
                        (apply #'order-forms parameters))
      (setf (gethash name *global-definitions*)
            (make-instance 'definition
                           :name name
                           :pattern pattern
                           :process (let ((*restricted-names* (cons name-variables *restricted-names*))
                                          (*process-variables* (cons process-variables *process-variables*)))
                                      (define-parallel-composition processes))))))
  +null-process+)

(defun define-named-concretion (name &rest arguments)
  "Even though this is in a process, it could match a definition where it
   expands into pattern position, so we try to patternize it if need be."
  (apply #'make-instance
         'named-concretion
         :name (if (listp name)
                   (define-parallel-composition (list name))
                   name)
         ;;  NB  We clear lexical scope because the expansion might
         ;;      introduce new names in between.
         :messages (let ((*reading-concretion-p* t)
                         (*restricted-names* ())
                         (*process-variables* ()))
                     (apply #'order-procs arguments))
         (unless *reading-concretion-p*
           (list :lexical-names *restricted-names*
                 :lexical-placeholders *process-variables*))))

(defgeneric eval (form)
  (:method ((form cons))
    (case (car form)
      (cont (let ((process (eval (second form))))
              (setf (continuation process)
                    (define-parallel-composition (cddr form)))
              process))
      (kell (apply #'define-kell (cdr form)))
      (message (apply #'define-message (cdr form)))
      (new (apply #'define-restriction (cdr form)))
      (par (define-parallel-composition (cdr form)))
      (trigger (apply #'define-trigger (cdr form)))
      (define (apply #'define-definition (cdr form)))
      ((process-variable name-variable) (error "Not a valid process."))
      (otherwise (apply #'define-named-concretion form))))
  (:method ((form symbol))
    (if *reading-name-p*
        (define-name form)
        (if (eq form 'null)
            +null-process+
            (define-placeholder form))))
  (:method ((form integer))
    (if *reading-name-p*
        (define-name form)
        form))
  (:method (form)
    form))

(defun read (&rest read-args)
  (let* ((*readtable* kilns::*kilns-readtable*)
         (*package* (find-package :kilns-user))
         (*read-eval* nil)
         (value (apply #'cl:read read-args)))
    (case value
      (null +null-process+)
      (otherwise value))))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace
                                &aux idx)
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (values
   (with-input-from-string (stream string :index idx :start start :end end)
     (if preserve-whitespace
       (read-preserving-whitespace stream eof-error-p eof-value)
       (read stream eof-error-p eof-value)))
   idx))

(defun dev ()
  (in-package :kilns)
  (setf *readtable* *kilns-readtable*)
  (values))
