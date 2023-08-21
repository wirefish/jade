;;;; An item is an entity that can generally by picked up or placed within a
;;;; container.

(in-package :jade)

;;; A prototype for items that defines generally-required attributes. Optional
;;; attributes that influence item behavior are described below.

(defclass item (entity) ())

(defentity item (&class item)
  (:brief "an item"
   :pose "is here."
   :full "The item is unremarkable."
   :icon pouch
   :size +small+
   :level 1
   :quantity 1
   :price nil
   :stackable nil))

;;; Items can be sorted based on their :item-group, :level, and :brief
;;; attributes.
;;;
;;; The :item-group attribute is a list where the first element denotes the
;;; group and the optional second element denotes a subgroup within that group.
;;;
;;; Groups are defined by calling set-item-groups. The argument is a list of
;;; lists. Each sublist contains a group label followed by zero or more subgroup
;;; labels.
;;;
;;; Items are first sorted in order by group in the order they appear in the
;;; list, then by subgroup in the order they appear in the group, then by
;;; :level, and finally by the noun-singular of their :brief attribute.

(defparameter *item-groups* nil)

(defun set-item-groups (groups)
  (setf *item-groups* groups))

(defmacro define-item-groups (&body groups)
  `(set-item-groups ',@groups))

(defun find-item-group (g)
  (loop for group in *item-groups* for i from 1
        when (eq g (car group)) do (return-from find-item-group (values i (cdr group))))
  0)

(defun item-subgroup-index (subgroup subgroups)
  (if-let ((i (position subgroup subgroups))) (1+ i) 0))

(defun item-sort-key (item)
  (with-attributes (item-group level) item
    (bind (((group &optional subgroup) (or (ensure-list item-group) '(nil)))
           (group-index subgroups (find-item-group group))
           (subgroup-index (item-subgroup-index subgroup subgroups)))
      (print (list item-group group subgroup level group-index subgroup-index))
      (+ (or level 0)
         (* 1000 subgroup-index)
         (* 1000000 group-index)))))

(defun item< (a b)
  "Returns true iff item `a' sorts before item `b'."
  (let ((k (- (? a :item-sort-key) (? b :item-sort-key))))
    (if (/= k 0)
        (< k 0)
        (string< (noun-singular (? a :brief)) (noun-singular (? b :brief))))))

;;; If the item is made of something in particular, the `:material' attribute is
;;; a noun describing that something. To facilitate this, defer parsing `:brief'
;;; until after the item has been created. Also, precompute the `:item-sort-key'
;;; attribute when the item is defined.

(defmethod transform-initval (class (name (eql :material)) value)
  (when value (parse-noun value)))

(defmethod transform-initval ((class (eql 'item)) (name (eql :brief)) value)
  (when value
    (if (find #\~ value)
        value
        (parse-noun value))))

(defmethod create-named-entity (label proto (class (eql 'item)) &rest attributes)
  (declare (ignore attributes))
  (let ((item (call-next-method)))
    (with-attributes (brief material) item
      (when (and material (stringp brief))
        (setf (? item :brief)
              (parse-noun (format nil brief
                                  (noun-article material)
                                  (noun-singular material))))))
    (setf (? item :item-sort-key) (item-sort-key item))
    item))

;;; The material can be substituted into the description of an item. The brief
;;; or full description is used as the control-string for format, with the
;;; indefinite article and singular of the material passed as arguments.

(defmethod describe-brief ((item item) &key quantity (article :indefinite) capitalize)
  (declare (ignore quantity article capitalize))
  (let ((brief (call-next-method)))
    (if-let ((material (? item :material)))
      (format nil brief (noun-article material) (noun-singular material))
      brief)))

(defmethod describe-full ((item item))
  (let ((description (call-next-method)))
    (if-let ((material (? item :material)))
      (format nil description (noun-article material) (noun-singular material))
      description)))

;;; If the item is associated with a quest, the `:quest' attribute is the label
;;; of the quest.

(defmethod transform-initval (class (name (eql :quest)) value)
  value)

;;; If the item is equippable, the `:equippable-slot' attribute describes where
;;; it can be equipped.

(defparameter *equippable-slots*
  '(:main-hand :off-hand :either-hand :both-hands :tool
    :head :torso :back :hands :waist :legs :feet :ears :neck :either-wrist :either-finger
    :backpack :belt-pouch))

;;; If the item can be purcased, its `:price' attribute is defined by a
;;; two-element list (quantity currency).

(defmethod transform-initval ((class (eql 'item)) (name (eql :price)) value)
  (when value
    (bind (((quantity currency) value))
      (clone-entity currency :quantity quantity))))

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

(defun find-item (container slot item &optional (quantity t))
  (find-if (lambda (e)
             (and (eq e item)
                  (or (eq quantity t) (>= (? e :quantity) quantity))))
           (? container slot)))

(defun remove-item-isa (container slot proto-label &optional (quantity t))
  (when-let ((item (find-item-isa container slot proto-label quantity)))
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

(defun insert-item (container slot item &key sorted)
  (or (find-if (lambda (x) (stack-item item x)) (? container slot))
      (progn
        (if sorted
            (setf (? container slot) (insert-sorted item (? container slot) #'item<))
            (push item (? container slot)))
        item)))
