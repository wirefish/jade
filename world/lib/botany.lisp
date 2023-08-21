(in-package :jade.lib)

;;; A prototype for resources gatherable using botany.

(defentity botany-resource (resource)
  (:required-skill botany
   :item-group (resource botany)))

;;; Tools.

(defentity sickle (item)
  (:brief "~a ~a sickle"
   :description "The ~*~a sickle is a basic botany tool."
   :icon sickle
   :equippable-slot :tool
   :item-group (tool botany)))

(defentity copper-sickle (sickle)
  (:level 1
   :material "copper"
   :price (10 silver-coin)))

(defentity bronze-sickle (sickle)
  (:level 5
   :material "bronze"
   :price (20 silver-coin)))

(defentity iron-sickle (sickle)
  (:level 10
   :material "iron"
   :price (40 silver-coin)))

;;; Nodes.

(defentity botany-node (resource-node)
  (:required-skill botany))

;;; Skills.

(defskill botany
  (:name "botany"
   :summary "Allows you to harvest plants."
   :price (20 silver-coin)
   :required-tool sickle))

;;; Trainer.

(defentity botany-trainer (trainer)
  (:teaches botany))
