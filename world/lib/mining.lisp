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
   :price (10 silver-coin)))

(defentity bronze-pickaxe (pickaxe)
  (:level 5
   :material "bronze"
   :price (20 silver-coin)))

(defentity iron-pickaxe (pickaxe)
  (:level 10
   :material "iron"
   :price (40 silver-coin)))

;;; TODO: Mineral deposits.

;;; Skills.

(defskill mining
  (:name "mining"
   :summary "Allows you to obtain ore and other resources from mineral
     deposits."
   :price (20 silver-coin)))

;;; Trainer.

(defentity mining-trainer (trainer)
  (:teaches mining))
