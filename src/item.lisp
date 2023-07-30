;;;; An item is an entity that can generally by picked up or placed within a
;;;; container.

(in-package :jade)

;;; A prototype for items that defines generally-required attributes. Optional
;;; attributes that define item behavior are described below.

(defentity item ()
  (:brief "an item"
   :pose "is here."
   :full "The item is unremarkable."
   :icon :pouch
   :size +small+
   :item-group :miscellany))

;;; The optional `:materials' attribute describes the composition of an item.
;;; Its value is a list of `material' structs.

(defstruct material
  label
  (name "something")
  (adjective nil)
  (tags nil)
  (traits nil)
  (level 0))

(defmacro defmaterial (label &body args)
  `(export (defparameter ,label
             (make-material :label ',label
                            ,@(loop for (key value) on args by #'cddr
                                    nconc (list key (transform-initval 'material key value)))))))

(defmethod transform-initval (type (name (eql :materials)) value)
  `(list ,@value))

(defmethod encode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'material-name value))

(defmethod decode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'symbol-value value))
