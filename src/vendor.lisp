(in-package :jade)

;;; A vendor is an entity with a `:sells' attribute, which is a list of item
;;; labels and quantities, where the quantity can be t for an unlimited supply.

(defclass vendor (entity) ())

(defentity vendor (&class vendor)
  (:brief "a vendor"
   :sells nil))

(defmethod clone-entity ((proto vendor) &rest attributes)
  (declare (ignore attributes))
  (let ((clone (call-next-method)))
    (setf (? clone :sells)
          (loop for (label quantity) on (? clone :sells) by #'cddr
                collect (clone-entity label :quantity quantity)))
    clone))

(defun vendors-in-location (location)
  (remove-if-not (lambda (x) (typep x 'vendor)) (? location :contents)))

;;; Specialize the receive event.

(defun buyer-can-pay (buyer item)
  (every (lambda (currency)
           (find-item-isa buyer :inventory
                          (entity-type currency)
                          (* (? item :quantity) (? currency :quantity))))
         (? item :cost)))

(defmethod receive :around ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (if (buyer-can-pay avatar item)
        (call-next-method)
        (progn
          (show avatar "You cannot pay for that.")
          nil))))

(defun pay-for-item (buyer item)
  (let (removed modified)
    (loop for currency in (? item :cost) do
      (let ((stack (find-item-isa buyer :inventory (entity-type currency))))
        (if (eq (remove-item buyer :inventory
                             stack
                             (* (? item :quantity) (? currency :quantity)))
                stack)
            (push stack removed)
            (push stack modified))))
    (update-inventory buyer modified removed)))

(defun reduce-vendor-stock (vendor item)
  (let ((stock (find-item-isa vendor :sells (entity-type item))))
    (assert stock)
    (unless (eq (? stock :quantity) t)
      (decf (? stock :quantity) (? item :quantity)))))

(defmethod receive ((avatar avatar) (vendor vendor) items)
  (let ((item (first items)))
    (pay-for-item avatar item)
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

(defun show-vendor-items (avatar vendor)
  (let ((items (remove-if-not (lambda (item)
                                (or (eq (? item :quantity) t)
                                    (> (? item :quantity) 0)))
                              (? vendor :sells))))
    (if items
        (show-links avatar
                    (format nil "~a is selling the following items:"
                            (describe-brief vendor :article :definite :capitalize t))
                    "look" ; FIXME:
                    (loop for item in items
                          collect (format nil "~a (~a each)"
                                          (describe-brief item :article nil)
                                          (format-list #'describe-brief
                                                       (? item :cost)))))
        (show avatar "~a doesn't have anything for sale at the moment."
              (describe-brief vendor :article :definite :capitalize t)))))

(defcommand buy (actor "buy" item "from" vendor)
  "Purchase items from a vendor. For example, `buy 3 apples from fruitseller`. If
*item* is omitted, you will see a list of the items sold by *vendor* and the
cost of each."
  (when-let ((vendor (match-vendor actor vendor "buy from")))
    (if item
        (bind ((item quantity (match-vendor-item actor item vendor)))
          (when item
            (receive actor vendor
                     (list (clone-entity (entity-proto item) :quantity quantity)))))
        (show-vendor-items actor vendor))))
