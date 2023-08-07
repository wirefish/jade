;;;; An item is an entity that can generally by picked up or placed within a
;;;; container.

(in-package :jade)

;;; A prototype for items that defines generally-required attributes. Optional
;;; attributes that influence item behavior are described below.

(defentity item ()
  (:brief "an item"
   :pose "is here."
   :full "The item is unremarkable."
   :icon :pouch
   :size +small+
   :level 0
   :item-group :miscellany))

;;; The optional `:materials' attribute describes the composition of an item.
;;; If present it must be a list of `material' structs.

(defstruct material
  label
  (name "something")
  (adjective nil)
  (tags nil)
  (traits nil)
  (level 0))

(defparameter *materials* (make-hash-table))

(defmacro defmaterial (label &body args)
  (with-gensyms (material)
    `(let ((,material (make-material :label ',label
                                    ,@(loop for (key value) on args by #'cddr
                                            nconc (list key (transform-initval key value))))))
       (sethash ',label *materials* ,material)
       ,material)))

(defmethod transform-initval ((name (eql :materials)) value)
  `(list ,@value))

(defmethod encode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'material-name value))

(defmethod decode-value ((entity entity) (name (eql :materials)) value)
  (mapcar #'symbol-value value))

;;; The optional `required-skill' attribute specifies a skill and minimum rank
;;; in that skill that are required in order to use the item. If present it must
;;; be a two-element list containing a skill and an integer rank.

;;;
