#+xcvb (module (:depends-on ("package" "process")))
(in-package :kilns)

(defclass multiset ()
  ((local-message-pattern :type (list local-message))
   (down-message-pattern :type (list down-message))
   (up-message-pattern :type (list up-message))
   (kell-message-pattern :type (list kell-message))))

(defclass pattern-language ()
  ((name)
   (grammar)))

(defgeneric message-match (language annotated-message message)
  (:documentation "This method needs to be defined for your pattern language and
                   each subclass of annotated-message."))

(defgeneric match (patterns messages)
  (:documentation "associates pairs consisting of a multiset of patterns drawn
                   from L and a multiset of annotated messages M with
                   substitutions (from names to names and from process variables
                   to processes). This matching relation is assumed to be
                   defined in terms of MESSAGE-MATCH, that defines how a single
                   pattern matches a single annotated message.")
  (:method ((patterns multiset) (messages multiset))
    (reduce #'union (reduce #'++ )))
  ;; FIXME: these need to ensure the message is local/up/down
  (:method ((patterns local-message) (messages message))
    (when (eq (parent (pattern local-message)) (parent message))
      (message-match *current-pattern-language* patterns messages)))
  (:method ((patterns down-message) (messages message))
    (when (member (parent messages)
    (message-match *current-pattern-language* patterns messages))
  (:method ((patterns up-message) (messages message))
    (message-match *current-pattern-language* patterns messages))
  (:method ((patterns kell-message) (messages kell))
    (message-match *current-pattern-language* patterns messages)))

(defclass annotated-message (pattern)
  ((name :type name)
   (process :type process)))

(defclass local-message (annotated-message)
  ())

(defclass down-message (annotated-message)
  ())

(defclass up-message (annotated-message)
  ())

(defclass up-message (annotated-message)
  ())

(defclass pattern ()
  ((multiset :type multiset)))

(defmethod free-names ((language pattern-language) (pattern pattern))
  )

(defmethod bound-name-variables ((language pattern-language) (pattern pattern))
  )

(defmethod bound-process-variables ((language pattern-language) (pattern pattern))
  )

(defmethod channel-names ((language pattern-language) (pattern pattern))
  ())

(defmethod structurally-congruent ((left pattern) (right pattern))
  (and (= (free-names left) (free-names right))
       (= (channel-names left) (channel-names right))
       (= (union (bound-name-variables left) (bound-process-variables left))
          (union (bound-name-variables right) (bound-process-variables right)))))