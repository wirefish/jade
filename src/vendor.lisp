(in-package :jade)

;;; A vendor is an entity with a `:sells' attribute, which is a list of labels
;;; defining items that are for sale.

(defclass vendor (entity) ())

(defentity vendor (&class vendor)
  (:brief "a vendor"
   :sells nil))

(defmethod transform-initval ((class (eql 'vendor)) (name (eql :sells)) value)
  (mapcar #'clone-entity value))

(defun vendors-in-location (location)
  (remove-if-not (lambda (x) (typep x 'vendor)) (? location :contents)))

;;; Specialize the receive event.

(defun actor-can-pay (actor price &optional (quantity 1))
  (find-item-isa actor :inventory (entity-type price) (* quantity (? price :quantity))))

(defun pay-vendor (actor vendor price &optional (quantity 1))
  "Assumes `actor-can-pay' returned true."
  (bind ((paid remaining (remove-item-isa actor :inventory (entity-type price)
                                          (* quantity (? price :quantity)))))
    (when paid
      (show actor "You give ~a to ~a." (describe-brief paid) (describe-brief vendor))
      (if remaining
          (update-inventory actor (list remaining) nil)
          (update-inventory actor nil (list paid))))))

(defmethod receive :around ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (if (actor-can-pay avatar (? item :price) (? item :quantity))
        (call-next-method)
        (progn
          (show avatar "You don't have enough ~a to pay for ~a."
                (describe-brief (? item :price) :quantity t)
                (describe-brief item))
          nil))))

(defmethod receive ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (pay-vendor avatar vendor (? item :price) (? item :quantity))
    (call-next-method)))

;;; Buy command.

(defun match-vendor (actor tokens verb)
  (match actor tokens :exactly-one (vendors-in-location (location actor))
   :no-tokens t
   :no-subjects "There are no vendors here."
   :no-match "There are no vendors here matching ~s."
   :multi-match (format nil "Do you want to ~a ~~a?" verb)))

(defun match-vendor-item (actor tokens vendor)
  "Returns two values: an item sold by `vendor' and the desired quantity of that item."
  (bind ((tokens quantity (split-quantity tokens))
         (quantity (or quantity 1))
         (items (find-matches tokens (can-see actor (? vendor :sells)))))
    (cond
      ((eq quantity t)
       (show actor "You can't buy all of an unlimited item.")
       nil)
      ((and (integerp quantity) (<= quantity 0))
       (show actor "You can't buy less than one of something.")
       nil)
      ((null items)
       (show actor "~a doesn't sell anything that matches ~s."
             (describe-brief vendor :article :definite :capitalize t)
             (join-tokens tokens))
       nil)
      ((second items)
       (show actor "Do you want to buy ~a?" (format-list #'describe-brief items "or"))
       nil)
      (t
       (values (first items) quantity)))))

(defclass buy-offer ()
  ((item :initarg :item)
   (vendor :initarg :vendor)))

(defmethod extend-offer (avatar (offer buy-offer))
  (with-slots (item vendor) offer
    (with-attributes (price quantity) item
      (show-notice
       avatar
       "~a has offered to sell you ~a for ~a. Type `accept` to complete the purchase."
       (describe-brief vendor :article :definite :capitalize t)
       (describe-brief item)
       (describe-brief price :quantity (* (? price :quantity) quantity))))))

(defmethod accept-offer (actor (offer buy-offer))
  (with-slots (item vendor) offer
    (receive actor vendor (list item))))

(defmethod reject-offer (actor (offer buy-offer))
  (with-slots (item) offer
    (show-notice actor "You have rejected the offer to purchase ~a."
                 (describe-brief item))))

(defcommand buy (actor "buy" item "from" vendor)
  "Purchase items from a vendor. For example, `buy 3 apples from fruitseller`. If
*item* is omitted, you will see a list of the items sold by *vendor* and the
price of each."
  (when-let ((vendor (match-vendor actor vendor "buy from")))
    (if item
        (bind ((item quantity (match-vendor-item actor item vendor)))
          (when item
            (if (or (? item :stackable) (eql quantity 1))
                (extend-offer actor (make-instance
                                     'buy-offer
                                     :item (clone-entity (entity-proto item) :quantity quantity)
                                     :vendor vendor))
                (show actor "You can only buy one ~a at a time."
                      (describe-brief item :article nil :quantity 1)))))
        (if-let ((items (remove-if (lambda (item) (eql (? item :quantity) 0))
                                   (? vendor :sells))))
          (show-vendor-items
           actor
           (format nil "~a is selling the following items:"
                   (describe-brief vendor :article :definite :capitalize t))
           vendor "buy" items)
          (show actor "~a doesn't have anything for sale at the moment."
                (describe-brief vendor :article :definite :capitalize t))))))
