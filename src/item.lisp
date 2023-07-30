;;;; An item is an entity that can generally by picked up or placed within a
;;;; container.

(in-package :jade)

;;; NOTE: An item will generally define a `:size' attribute; see entity.lisp for
;;; potential values.

;;; The `:materials' attribute describes the composition of an item. Its value
;;; is a list of `material' structs. A material represents an uncountable
;;; concept such as "metal" or "air", not something that can be directly
;;; instantiated.

(defstruct material
  name (tags nil) (traits nil) (level 0))

(defmacro defmaterial (name &body args)
  `(defparameter ,name
     (make-material :name ',name ,@args)))

(defmethod encode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'material-name value))

(defmethod decode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'symbol-value value))

;;; TEST:

(defmaterial gold
  :tags '(:metal :soft)
  :traits '((:fire-resist -1) (:charisma 1))
  :level 12)

(defentity item ()
  (:brief "an item"
   :pose "is here."
   :full "The item is unremarkable."
   :icon :pouch
   :size +small+
   :item-group :miscellany))
