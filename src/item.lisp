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
   :item-group :miscellany
   :quantity 1
   :stackable nil))

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

;;; If the item is equippable, the `equippable-slot' attribute describes where
;;; it can be equipped.

(defparameter *equippable-slots*
  '(:main-hand :off-hand :either-hand :both-hands :tool
    :head :torso :back :hands :waist :legs :feet :ears :neck :either-wrist :either-finger
    :backpack :belt-pouch)
  "Descriptions of how an item may be equipped.")

;;; The optional `required-skill' attribute specifies a skill and minimum rank
;;; in that skill that are required in order to use the item. If present it must
;;; be a two-element list containing a skill and an integer rank.

;;; Items can be stacked if they have the same prototype and their `stackable'
;;; attribute is not null. If `stackable' is t, there is no limit on how many
;;; items can be stacked together; if it is a number, then only that many can be
;;; stacked.

(defun split-item (item count)
  "Modifies `item' to reduce its quantity by `count' and returns a new entity that
represents `count' of the same item. Returns nil if entity cannot be split."
  (when-attributes (quantity) item
    (when (> quantity count)
      (prog1
          (clone-entity (entity-proto item) :quantity count)
        (decf (? item :quantity) count)))))

(defun remove-item (container slot item &optional (quantity t))
  (if (or (eq quantity t) (= quantity (? item :quantity)))
      (progn
        (deletef (? container slot) item)
        item)
      (split-item item quantity)))

(defun stackable-p (item stack)
  (when (eq (entity-proto item) (entity-proto stack))
    (when-attributes (stackable) stack
      (or (eq stackable t)
          (<= (+ (? item :quantity) (? stack :quantity)) stackable)))))

(defun can-insert-item (container slot item)
  (let ((stack-limit (? item :stackable)))
    (or (null stack-limit)
        (eq stack-limit t)
        (let ((stack (find-if (lambda (x) (eq (entity-proto item) (entity-proto x)))
                              (? container slot))))
          (or (null stack) (stackable-p item stack))))))

(defun stack-item (item stack)
  "Attempts to merge `item' into `stack'. Returns the modified `stack' on success."
  (when (stackable-p item stack)
    (incf (? stack :quantity) (? item :quantity))
    stack))

(defun insert-item (container slot item)
  (or (find-if (lambda (x) (stack-item item x)) (? container slot))
      (progn
        (push item (? container slot))
        item)))
