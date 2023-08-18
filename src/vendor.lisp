(in-package :jade)

;;; A vendor is an entity with a `:sells' attribute, which is a list describing
;;; items that are for sale. Each list element can be a label, indicating an
;;; item available in unlimited quantity, or a list (label quantity).

(defclass vendor (entity) ())

(defentity vendor (&class vendor)
  (:brief "a vendor"
   :sells nil))

(defmethod clone-entity ((proto vendor) &rest attributes)
  (declare (ignore attributes))
  (let ((clone (call-next-method)))
    (setf (? clone :sells)
          (loop for info in (? clone :sells)
                collect (bind (((label &optional (quantity t)) (ensure-list info)))
                          (clone-entity label :quantity quantity))))
    clone))

(defun vendors-in-location (location)
  (remove-if-not (lambda (x) (typep x 'vendor)) (? location :contents)))

;;; Specialize the receive event.

(defun buyer-can-pay (buyer item)
  (let ((price (? item :price)))
    (find-item-isa buyer :inventory
                   (entity-type price)
                   (* (? item :quantity) (? price :quantity)))))

(defmethod receive :around ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (if (buyer-can-pay avatar item)
        (call-next-method)
        (progn
          (show avatar "You cannot pay for that.")
          nil))))

(defun pay-for-item (buyer vendor item)
  (let* ((price (? item :price))
         (stack (find-item-isa buyer :inventory (entity-type price)))
         (paid (remove-item buyer :inventory stack
                            (* (? item :quantity) (? price :quantity)))))
    (show buyer "You give ~a ~a."
          (describe-brief vendor)
          (describe-brief paid))
    (if (eq paid stack)
        (update-inventory buyer nil (list stack))
        (update-inventory buyer (list stack) nil))))

(defun reduce-vendor-stock (vendor item)
  (let ((stock (find-item-isa vendor :sells (entity-type item))))
    (assert stock)
    (unless (eq (? stock :quantity) t)
      (decf (? stock :quantity) (? item :quantity)))))

(defmethod receive ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (pay-for-item avatar vendor item)
    (reduce-vendor-stock vendor item)
    (call-next-method)))

;;; Buy command.

(defun match-vendor (actor tokens verb)
  (match actor tokens :exactly-one (vendors-in-location (location actor))
   :no-tokens t
   :no-subjects "There are no vendors here."
   :no-match "There are no vendors here matching ~s."
   :multi-match (format nil "Do you want to ~a ~~a?" verb)))

(defun quantity-available (actor vendor item desired-quantity)
  (cond
    ((eq (? item :quantity) t)
     (if (eq desired-quantity t)
         (progn
           (show actor "You cannot buy all of an unlimited item.")
           nil)
         desired-quantity))
    ((= (? item :quantity) 0)
     (show actor "~a is out of ~a."
           (describe-brief vendor :article :definite :capitalize t)
           (describe-brief item :quantity t))
     nil)
    ((eq desired-quantity t)
     (? item :quantity))
    ((<= desired-quantity (? item :quantity))
     desired-quantity)
    (t
     (show actor "~a only has ~a."
           (describe-brief vendor :article :definite :capitalize t)
           (describe-brief item))
     nil)))

(defun match-vendor-item (actor tokens vendor)
    (bind ((tokens quantity (split-quantity tokens))
           (quantity (or quantity 1))
           (items (find-matches tokens (? vendor :sells))))
      (case (length items)
        (0
         (show actor "~a doesn't sell anything that matches ~s."
               (describe-brief vendor :article :definite :capitalize t)
               (join-tokens tokens))
         nil)
        (1
         (when-let ((quantity (quantity-available actor vendor (first items) quantity)))
           (values (first items) quantity)))
        (t
         (show actor "Do you want to buy ~a?"
               (format-list #'describe-brief items "or"))
         nil))))

(defclass buy-offer ()
  ((item :initarg :item)
   (vendor :initarg :vendor)))

(defmethod extend-offer (avatar (offer buy-offer))
  (with-slots (item vendor) offer
    (let* ((currency (? item :price))
           (total-price (* (? item :quantity) (? currency :quantity))))
      (show-notice
       avatar
       "~a has offered to sell you ~a for ~a. Type `accept` to complete the purchase."
       (describe-brief vendor :article :definite :capitalize t)
       (describe-brief item)
       (describe-brief currency :quantity total-price)))))

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
