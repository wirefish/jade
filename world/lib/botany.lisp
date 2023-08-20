(in-package :jade.lib)

;;; Tools.

(defentity sickle (item)
  (:brief "~a ~a sickle"
   :description "The ~*~a sickle is a basic botany tool."
   :icon sickle
   :equippable-slot :tool))

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

;;; Skills.

(defskill botany
  (:name "botany"
   :summary "Allows you to harvest plants."
   :price (20 silver-coin)
   :required-tool sickle))

;;; Trainer.

(defentity botany-trainer (trainer)
  (:teaches botany))
