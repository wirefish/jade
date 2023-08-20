(in-package :jade.lib)

;;; Ore.

(defentity ore (resource)
  (:brief "a chunk[s] of ~*~a ore"
   :description "The ore is a small piece of ~*~a-bearing rock. It can be
     smelted to obtain ~:*~a in its pure form."
   :stackable t
   :required-skill mining))

(defentity copper-ore (ore)
  (:material "copper"
   :required-rank 1))

(defentity tin-ore (ore)
  (:material "tin"
   :required-rank 10))

(defentity zinc-ore (ore)
  (:material "zinc"
   :required-rank 20))

(defentity silver-ore (ore)
  (:material "silver"
   :required-rank 30))

(defentity iron-ore (ore)
  (:material "iron"
   :required-rank 40))

(defentity gold-ore (ore)
  (:material "gold"
   :required-rank 50))

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
   :price (20 silver-coin)
   :required-tool pickaxe))

;;; Trainer.

(defentity mining-trainer (trainer)
  (:teaches mining))
