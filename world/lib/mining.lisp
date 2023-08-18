(in-package :jade.lib)

;;; Ore.

(defentity ore (item)
  (:brief "a chunk[s] of ~*~a ore"
   :description "The ore is a small piece of ~*~a-bearing rock. It can be
     smelted to obtain ~:*~a in its pure form."
   :stackable t))

(defentity copper-ore (ore)
  (:material "copper"))

(defentity tin-ore (ore)
  (:material "tin"))

(defentity zinc-ore (ore)
  (:material "zinc"))

(defentity silver-ore (ore)
  (:material "silver"))

(defentity gold-ore (ore)
  (:material "gold"))

(defentity iron-ore (ore)
  (:material "iron"))

;;; TODO: Other mineable things.

;;; Tools.

(defentity pickaxe (item)
  (:brief "~a ~a pickaxe"
   :description "The ~*~a pickaxe is a basic mining tool."
   :icon pickaxe
   :equippable-slot :tool))

(defentity copper-pickaxe (pickaxe)
  (:level 1
   :material "copper"
   :cost (silver-coin 10)))

(defentity bronze-pickaxe (pickaxe)
  (:level 5
   :material "bronze"
   :cost (silver-coin 20)))

(defentity iron-pickaxe (pickaxe)
  (:level 10
   :material "iron"
   :cost (silver-coin 40)))

;;; TODO: Ore deposits.
