;;;; An item is an entity that can generally by picked up or placed within a
;;;; container.

(in-package :jade)

;;; A material is an entity that represents a substance that goes into the
;;; construction of an item. It is an uncountable noun, such as "wood" or
;;; "metal".

;; FIXME: make a material class that inherits from some new base class for
;; entity that only has label and attributes -- named-object?

(defentity material ()
  (:name "something"
   :adjective nil
   :level 1
   :tags nil
   :traits nil
   :durability 1.0))

(defmacro defmaterial (label (&optional proto) attributes &body behaviors)
  `(defentity ,label (,(or proto 'material)) ,attributes ,@behaviors))

(defmethod transform-initval ((name (eql :tags)) value)
  `(quote ,value))

(defmethod transform-initval ((name (eql :traits)) value)
  `(quote ,value))

(defun material-has-tag (material tag)
  (with-slots (label tags) material
      (or (eq tag label) (position tag tags))))

;;; A prototype for items that defines generally-required attributes. Optional
;;; attributes that influence item behavior are described below.

(defclass item (entity) ())

(defentity item (&class item)
  (:brief "an item"
   :pose "is here."
   :full "The item is unremarkable."
   :icon :pouch
   :size +small+
   :level 1
   :item-group :miscellany
   :quantity 1
   :stackable nil))

;;; The optional `:materials' attribute describes the composition of an item.
;;; If present it must be a list of symbols bound to `material' structs.

(defmethod transform-initval ((name (eql :materials)) value)
  `(quote ,value))

(defmethod describe-brief ((item item) &key quantity (article :indefinite) capitalize)
  (declare (ignore quantity article capitalize))
  (apply #'format nil (call-next-method)
         (mapcar (lambda (symbol)
                   (let ((material (symbol-value symbol)))
                     (or (? material :adjective) (? material :name))))
                 (? item :materials))))

;;; If the item is associated with a quest, the `quest' attribute is the label
;;; of the quest.

(defmethod transform-initval ((name (eql :quest)) value)
  `(quote ,value))

(defmethod encode-value ((entity item) (name (eql :quest)) value)
  (quest-label value))

(defmethod decode-value ((entity item) (name (eql :quest)) value)
  (symbol-value value))

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

;;; The optional `craft-skill' attribute specifies a skill and minimum rank in
;;; that skill that are required in order to craft the item. If present it must
;;; be a two-element list containing a skill and an integer rank. In addition,
;;; the `craft-materials' attribute is a list (count material-tag...) lists.

(defmethod transform-initval ((name (eql :craft-skill)) value)
  `(list ,@value))

(defmethod transform-initval ((name (eql :craft-materials)) value)
  `(quote ,value))

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

(defun remove-items-if (container slot pred)
  (bind ((kept removed (loop for item in (? container slot)
                             if (funcall pred item)
                               collect item into removed
                             else
                               collect item into kept
                             finally (return (values kept removed)))))
    (setf (? container slot) kept)
    removed))

(defun contains-isa (container slot label)
  (some (lambda (e) (entity-isa e label)) (? container slot)))

(defun find-item-isa (container slot proto-label &optional (quantity t))
  (find-if (lambda (e)
             (and (entity-isa e proto-label)
                  (or (eq quantity t) (>= (? e :quantity) quantity))))
           (? container slot)))

(defun remove-item-isa (container slot proto-label &optional (quantity t))
  (when-let ((item (find-item-with-proto container slot proto-label quantity)))
    (remove-item container slot item quantity)))

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
