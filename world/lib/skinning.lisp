(in-package :jade.lib)

;;; A prototype for resources gatherable using skinning.

(defentity skinning-resource (resource)
  (:required-skill skinning
   :item-group (resource skinning)))

;;; For skinning, corpses act as resource nodes.

(defentity skinnable-corpse (corpse)
  (:required-skill skinning))

;;; Tools.

(defentity skinning-knife (item)
  (:brief "~a ~a skinning knife"
   :description "The ~*~a skinning knife is a basic skinning tool."
   :icon skinning-knife
   :equippable-slot :tool
   :item-group (tool skinning)))

(defentity copper-skinning-knife (skinning-knife)
  (:level 1
   :material "copper"
   :price (10 silver-coin)))

(defentity bronze-skinning-knife (skinning-knife)
  (:level 5
   :material "bronze"
   :price (20 silver-coin)))

(defentity iron-skinning-knife (skinning-knife)
  (:level 10
   :material "iron"
   :price (40 silver-coin)))

;;; Skills.

(defskill skinning
  (:name "skinning"
   :summary "Allows you to obtain hides from the corpses of certain creatures."
   :price (20 silver-coin)
   :required-tool skinning-knife))

;;; Trainer.

(defentity skinning-trainer (trainer)
  (:teaches skinning))
