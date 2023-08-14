(in-package :jade)

(defvar *entity-id-counter* 0)

(defclass entity ()
  ((id :initarg :id :initform (incf *entity-id-counter*) :reader entity-id)
   (label :initarg :label :initform nil :reader entity-label)
   (proto :initarg :proto :initform nil :reader entity-proto)
   (ancestry :initarg :ancestry :initform nil :reader entity-ancestry)
   (container :initarg :container :initform nil :accessor entity-container)
   (attributes :initarg :attributes :initform (make-hash-table) :accessor entity-attributes)
   (behavior :initarg :behavior :initform nil :accessor entity-behavior)
   (activity :initarg :activity :initform nil :accessor entity-activity)))

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

(defun create-named-entity (label proto class &rest attributes)
  "Creates a named entity with a given prototype."
  (let* ((proto (symbol-value proto))
         (entity (make-instance (or class (if proto (type-of proto) 'entity))
                               :label label :proto proto)))
    (when-let ((b (and proto (slot-value proto 'behavior))))
      (setf (entity-behavior entity) (copy-hash-table b)))
    (apply #'sethash* (slot-value entity 'attributes) attributes)
    (set label entity)
    (export label)
    entity))

(defgeneric clone-entity (proto &rest attributes)
  (:documentation "Creates an new anonymous entity with `proto' as its prototype."))

(defmethod clone-entity ((proto-name symbol) &rest attributes)
  (if-let ((proto (symbol-value-or-nil proto-name)))
    (apply #'clone-entity proto attributes)
    (error "unknown prototype ~s" proto-name)))

(defmethod clone-entity ((proto entity) &rest attributes)
  (let ((clone (make-instance (type-of proto) :proto proto :behavior (entity-behavior proto))))
    (apply #'sethash* (slot-value clone 'attributes) attributes)
    clone))

;;; A mechanism for transforming initializer forms in `defentity' and similar
;;; macros.

(defgeneric transform-initval (name expr)
  (:method (name expr)
    (typecase expr
      (list (if (eql (car expr) 'quote) expr `(list ,@expr)))
      (t expr))))


(defmacro defentity (name (&rest proto-spec) attributes &body behavior)
  (let* ((pos (position '&class proto-spec))
         (proto (unless (eql pos 0) (first proto-spec)))
         (class (when (numberp pos) (elt proto-spec (1+ pos)))))
    (with-gensyms (entity)
      `(let ((,entity (create-named-entity
                       ',name ',proto ',class
                       ,@(loop for (key value) on attributes by #'cddr
                               nconc (list key
                                           (transform-initval key value))))))
         ,@(when behavior `((defbehavior ,entity ,@behavior)))
         ,entity))))

;;; Working with entity attributes.

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))
  (if (keywordp slot-name)
      (with-slots (attributes proto) instance
        (multiple-value-bind (value found) (gethash slot-name attributes)
          (if found value (when proto (slot-value proto slot-name)))))
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
  (if-let ((proto (symbol-value-or-nil (first data))))
    (let ((entity (make-instance (type-of proto) :proto proto)))
      (loop for (name slot-data) on (rest data) by #'cddr do
        (setf (slot-value entity name) (decode-value entity name slot-data)))
      entity)
    (progn
      (format-log :warning "cannot decode entity with unknown prototype ~a"
                  (first data))
      nil)))

;;; Standard entity attributes. Each may define a transform method used when
;;; parsing the attribute's value in `defentity' and similar macros, or a set of
;;; expected values.

;; The `:brief' attribute is a noun phrase used to describe the entity when it
;; does not have a proper name. It is also used when matching against user
;; input.
(defmethod transform-initval ((name (eql :brief)) value)
  `(when ,value (parse-noun ,value)))

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

(defgeneric describe-brief (entity &key quantity article capitalize)
  (:documentation "Returns a string that briefly describes an entity."))

(defmethod describe-brief ((entity null) &key quantity article capitalize)
  (declare (ignore quantity article capitalize))
  "<null>")

(defparameter *default-brief* (parse-noun "an entity"))

(defmethod describe-brief ((entity entity) &key quantity (article :indefinite) capitalize)
  (or (? entity :name)
      (format-noun (or (? entity :brief) *default-brief*)
                   :quantity (or quantity (? entity :quantity) 1)
                   :article article :capitalize capitalize)))

(defparameter *default-pose* (parse-verb "is here."))

(defun describe-pose (entity &key quantity)
  (format-verb (or (? entity :pose) *default-pose*)
               :quantity (or quantity (? entity :quantity) 1)))

(defgeneric describe-full (entity))

(defmethod describe-full ((entity entity))
  (or (? entity :description)
      (format nil "~a is unexceptional in every way."
              (describe-brief entity :article :definite :capitalize t))))

(defun get-icon (entity)
  (or (? entity :icon) (? entity :race :icon)))
