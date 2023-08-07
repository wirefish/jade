(in-package :jade)

(defvar *entity-id-counter* 0)

(defclass entity ()
  ((id :initarg :id :initform (incf *entity-id-counter*) :reader entity-id)
   (label :initarg :label :initform nil :reader entity-label)
   (proto :initarg :proto :initform nil :reader entity-proto)
   (ancestry :initarg :ancestry :initform nil :reader entity-ancestry)
   (container :initarg :container :initform nil :accessor entity-container)
   (attributes :initarg :attributes :initform (make-hash-table) :accessor entity-attributes)
   (behavior :initform nil :accessor entity-behavior)))

(defmethod initialize-instance :after ((entity entity) &key)
  (with-slots (label proto ancestry) entity
    (if proto
        (with-slots ((proto-ancestry ancestry)) proto
          (setf ancestry (if label (cons label proto-ancestry) proto-ancestry)))
        (when label (setf ancestry (list label))))))

(defmethod print-object ((obj entity) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (id label proto) obj
      (write (or label (list (entity-label proto) id)) :stream stream))))

(defun entity-isa (entity label)
  (numberp (position label (slot-value entity 'ancestry))))

(defvar *named-entities* (make-hash-table :size 5000))

(defun find-entity (label &optional default)
  (gethash label *named-entities* default))

(defun define-entity (label proto &rest keys-values)
  "Creates a named entity with a given prototype."
  (let ((entity (make-instance (if proto (type-of proto) 'entity)
                               :label label :proto proto)))
    (apply #'sethash* (slot-value entity 'attributes) keys-values)
    (sethash label *named-entities* entity)
    entity))

(defun clone-entity (entity &rest keys-values)
  "Creates an anonymous entity (i.e. one without a label) with `entity' as its
prototype and attributes initialized from `keys-values'."
  (let* ((proto (if (symbolp entity) (find-entity entity) entity))
         (clone (make-instance (type-of proto) :proto proto)))
    (apply #'sethash* (slot-value clone 'attributes) keys-values)
    clone))

;;; A mechanism for transforming initializer forms in `defproto' and similar
;;; macros.

(defgeneric transform-initval (name expr)
  (:method (name expr)
    (typecase expr
      (list (if (eql (car expr) 'quote) expr `(list ,@expr)))
      (t expr))))

(defmacro defentity (name (&optional proto-name) attributes &body behavior)
  (with-gensyms (proto entity)
    `(let* ((,proto ,(when proto-name `(find-entity ',proto-name)))
            (,entity (define-entity ',name ,proto
                       ,@(loop for (key value) on attributes by #'cddr
                               nconc (list key
                                           (transform-initval key value))))))
       ,@(when behavior `((defbehavior ,entity ,@behavior)))
       (export ',name)
       ,entity)))

;;; Working with entity attributes.

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))
  (if (keywordp slot-name)
      (with-slots (attributes proto) instance
        (let ((value (gethash slot-name attributes)))
          (or value (when proto (slot-value proto slot-name)))))
      (call-next-method)))

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'setf)) &optional new-value)
  (if (keywordp slot-name)
      (with-slots (attributes) instance
        (sethash slot-name attributes new-value))
      (call-next-method)))

(defun has-attributes (entity &rest names)
  (with-slots (attributes) entity
    (loop for name in names do
      (multiple-value-bind (value found) (gethash name attributes)
        (declare (ignore value))
        (unless found
          (return-from has-attributes nil))))
    t))

(defmacro with-attributes ((&rest attrs) entity &body body)
  "Executes `body' with variables bound to attributes of `entity'. Each
attribute is specified by either a symbol or a two-element list. In the latter
case, the first element is a symbol and the second is a default value if the
attribute's value is nil."
  `(let ,(loop for attr in attrs
               collect
               (typecase attr
                 (symbol `(,attr (? ,entity ,(make-keyword attr))))
                 (list `(,(first attr) (or (? ,entity ,(make-keyword (first attr)))
                                           ,(second attr))))))
     ,@body))

(defmacro when-attributes ((&rest attrs) entity &body body)
  (let ((all-attrs (remove '&optional attrs))
        (required-attrs (subseq attrs 0 (position '&optional attrs))))
    `(with-attributes (,@all-attrs) ,entity
       (when (and ,@required-attrs)
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
  (let* ((proto (find-entity (first data)))
         (initargs (rest data))
         (entity (make-instance (type-of proto) :proto proto)))
    (loop for (name slot-data) on initargs by #'cddr do
      (setf (slot-value entity name) (decode-value entity name slot-data)))
    entity))

;;; Standard entity attributes. Each may define a transform method used when
;;; parsing the attribute's value in `defentity' and similar macros, or a set of
;;; expected values.

;; The `:brief' attribute is a noun phrase used to describe the entity when it
;; does not have a proper name. It is also used when matching against user
;; input.
(defmethod transform-initval ((name (eql :brief)) value)
  `(parse-noun ,value))

;; The `:pose' attribute is a verb phrase that describes how observers see the
;; entity, e.g. "is standing against the wall." Note the trailing punctuation is
;; included.
(defmethod transform-initval ((name (eql :pose)) value)
  `(parse-verb ,value))

;; The `:alts' attribute is an optional list of additional noun phrases which
;; can be used when matching the entity against user input.
(defmethod transform-initval ((name (eql :alts)) value)
  `(list ,@(mapcar (lambda (x) `(parse-noun ,x)) value)))

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

;;;

(defvar *default-brief* (parse-noun "an entity"))

(defun describe-brief (entity &key (quantity 1) (article :indefinite) capitalize)
  (or (? entity :name)
      (format-noun (or (? entity :brief) (? entity :race :brief) *default-brief*)
                   :quantity quantity :article article :capitalize capitalize)))

(defvar *default-pose* (parse-verb "is here."))

(defun describe-pose (entity &key (quantity 1))
  (format-verb (or (? entity :pose) *default-pose*) :quantity quantity))

(defun describe-full (entity)
  (or (? entity :full)
      (format nil "~a is unexceptional in every way."
              (describe-brief entity :article :definite :capitalize t))))
