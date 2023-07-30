(in-package :jade)

(defvar *entity-id-counter* 0)

(defclass entity ()
  ((id :initarg :id :initform (incf *entity-id-counter*) :reader entity-id)
   (label :initarg :label :initform nil :reader entity-label)
   (proto :initarg :proto :initform nil :reader entity-proto)
   (ancestry :initarg :ancestry :initform nil :reader entity-ancestry)
   (container :initarg :container :initform nil)
   (attributes :initarg :attributes :initform (make-hash-table))
   (behaviors :initform nil)))

(defmethod initialize-instance :after ((entity entity) &key)
  (with-slots (label proto ancestry) entity
    (if proto
        (with-slots ((proto-ancestry ancestry)) proto
          (setf ancestry (if label (cons label proto-ancestry) proto-ancestry)))
        (when label (setf ancestry (list label))))))

(defun entity-isa (entity label)
  (numberp (position label (slot-value entity 'ancestry))))

(defun clone-entity (entity &rest keys-values)
  "Creates an anonymous entity (i.e. one without a label) with `entity' as its
prototype and attributes initialized from `keys-values'."
  (let ((clone (make-instance (type-of entity) :proto entity)))
    (apply #'sethash* (slot-value clone 'attributes) keys-values)
    clone))

(defvar *attribute-transforms* (make-hash-table :test 'eq))

(defun transform-attributes (&rest keys-values)
  (let ((attributes (make-hash-table)))
    (loop for (key value) on keys-values by #'cddr do
      (sethash key attributes
               (funcall (or (gethash key *attribute-transforms*) #'identity) value)))
    attributes))

(defmacro defentity (name (&optional base-name) attributes &body behaviors)
  `(progn
     (defparameter ,name
       (make-instance 'entity
                      :label ',name
                      :proto ,base-name
                      :attributes (transform-attributes ,@attributes)))
     (defbehaviors ,name ,@behaviors)
     (export ',name)
     ,name))

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))
  (with-slots (attributes proto) instance
    (let ((value (gethash slot-name attributes)))
      (or value (when proto (slot-value proto slot-name))))))

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'setf)) &optional new-value)
  (with-slots (attributes) instance
    (sethash slot-name attributes new-value)))

(defmacro with-attributes ((&rest names) entity &body body)
  (let ((all-names (remove '&optional names))
        (required-names (subseq names 0 (position '&optional names))))
    `(with-slots (,@all-names) ,entity
       (when (and ,@required-names)
         ,@body))))

;;; Encoding and decoding.

(defgeneric encode-value (entity name value)
  (:documentation "Returns the encoded form of `value' for the slot or attribute named `name' in
entity `entity'.")
  (:method ((entity entity) name value)
    value))

(defgeneric encoded-slots (entity)
  (:documentation "Returns a list of symbols naming slots to encode for entity `entity'. The
'proto slot and all attributes are always encoded.")
  (:method ((entity entity))
    nil))

(defun encode-entity (entity)
  "Encodes an entity, including values for slots selected by `encoded-slots' and
all attributes."
  ;; Only anonymous entities (i.e. those created with `clone-entity') should
  ;; ever be encoded.
  (assert (and (null (entity-label entity)) (entity-proto entity)))
  (cons (first (entity-ancestry entity))
        (nconc
         (loop for name in (encoded-slots entity)
               nconc (list name (encode-value entity name (slot-value entity name))))
         (with-slots (attributes) entity
           (loop for key being the hash-keys in attributes using (hash-value value)
               nconc (list key (encode-value entity key (gethash key attributes))))))))

(defgeneric decode-value (entity name data)
  (:documentation "Returns the result of decoding `data' into a value for the slot or attribute
  named `name' in entity `entity'.")
  (:method ((entity entity) name data)
    data))

(defun decode-entity (data)
  (let* ((proto (symbol-value (first data)))
         (initargs (rest data))
         (entity (make-instance (type-of proto) :proto proto)))
    (loop for (name slot-data) on initargs by #'cddr do
      (setf (slot-value entity name) (decode-value entity name slot-data)))
    entity))

;;; Standard entity attributes. Each may define a transform used when parsing
;;; the attribute's value in `defentity', or a set of expected values.

;; The `:brief' attribute is a noun phrase used to describe the entity when it
;; does not have a proper name. It is also used when matching against user
;; input.
(sethash :brief *attribute-transforms* #'parse-noun)

;; The `:pose' attribute is a verb phrase that describes how observers see the
;; entity, e.g. "is standing against the wall." Note the trailing punctuation is
;; included.
(sethash :pose *attribute-transforms* #'parse-verb)

;; The `:alts' attribute is an optional list of additional noun phrases which
;; can be used when matching the entity against user input.
(sethash :alts *attribute-transforms* (lambda (x) (mapcar #'parse-noun x)))

;; Possible values of the `:size' attribute, with examples of how they apply to
;; different classes of entities. The values are a rough representation of
;; relative volume between entities in the same class.
(defconstant +miniscule+ 1/512 "a butterfly; a coin")
(defconstant +tiny+ 1/64 "a bird; a necklace")
(defconstant +small+ 1/8 "a small dog; a dagger; a shoe")
(defconstant +medium+ 1 "a human; a sword; a breastplate")
(defconstant +large+ 8 "a troll; a treasure chest")
(defconstant +huge+ 64 "a storm giant; a four-poster bed")
(defconstant +gigantic+ 512 "a titan; a house")
