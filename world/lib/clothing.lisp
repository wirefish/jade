(in-package :jade.lib)

;;; Common clothing items.

(defentity shirt (item)
  (:brief "a ~a shirt"
   :description "The ~a shirt has mid-length sleeves and extends slightly below
     the waist when worn."
   :icon 'shirt
   :craft-materials ((3 cloth))
   :equippable-slot :torso))

(defentity pants (item)
  (:brief "a pair of ~a pants"
   :description "The ~a pants are loose-fitting and comfortable."
   :icon 'pants
   :craft-materials ((3 cloth))
   :equippable-slot :legs))

(defentity shoes (item)
  (:brief "a pair of ~a shoes"
   :description "The low ~a shoes have a sturdy sole. They seem suitable for
     walking long distances."
   :icon 'shoes
   :craft-materials ((3 leather))
   :equippable-slot :feet))

;;; Backpacks.

(defentity small-backpack (item)
  (:brief "a small ~a backpack"
   :description "The small ~a backpack has comfortable straps and a drawstring
     closure."
   :icon 'small-pack
   :craft-materials ((3 cloth))
   :equippable-slot :backpack
   :capacity 20))
