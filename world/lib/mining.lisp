(in-package :jade.lib)

;;; Ore.

(defentity ore (item)
  (:brief "a chunk[s] of ~*~a ore"
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
  (:brief "a pickaxe"
   :icon pickaxe
   :equippable-slot :tool))

(defentity copper-pickaxe (pickaxe)
  (:brief "a copper pickaxe"
   :level 1
   :cost (silver-coin 5)))

(defentity bronze-pickaxe (pickaxe)
  (:brief "a bronze pickaxe"
   :level 5
   :cost (silver-coin 10)))

;;; TODO: Ore deposits.
