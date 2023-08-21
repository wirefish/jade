(in-package :jade.lib)

;;; Common clothing items.

(defentity shirt (item)
  (:brief "a ~a shirt"
   :description "The ~a shirt has mid-length sleeves and extends slightly below
     the waist when worn."
   :icon shirt
   :equippable-slot :torso
   :item-group (armor torso)))

(defentity pants (item)
  (:brief "a pair of ~a pants"
   :description "The ~a pants are loose-fitting and comfortable."
   :icon pants
   :equippable-slot :legs
   :item-group (armor legs)))

(defentity shoes (item)
  (:brief "a pair of ~a shoes"
   :description "The low ~a shoes have a sturdy sole. They seem suitable for
     walking long distances."
   :icon shoes
   :equippable-slot :feet
   :item-group (armor feet)))

;;; Backpacks.

(defentity small-backpack (item)
  (:brief "a small ~a backpack"
   :description "The small ~a backpack has comfortable straps and a drawstring
     closure."
   :icon small-pack
   :equippable-slot :backpack
   :capacity 20
   :item-group (storage backpack)))
