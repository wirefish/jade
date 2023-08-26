(in-package :jade.lib)

;;; A prototype for resources gatherable using botany.

(defentity logging-resource (resource)
  (:required-skill logging
   :item-group (resource logging)))

;;; Nodes.

(defentity logging-node (resource-node)
  (:required-skill logging))

;;; Tools.

(defentity logging-axe (item)
  (:brief "~a ~a logging axe"
   :description "The ~*~a logging axe is a basic logging tool."
   :icon logging-axe
   :equippable-slot :tool))

(defentity copper-logging-axe (logging-axe)
  (:level 1
   :material "copper"
   :price (10 silver-coin)))

(defentity bronze-logging-axe (logging-axe)
  (:level 5
   :material "bronze"
   :price (20 silver-coin)))

(defentity iron-logging-axe (logging-axe)
  (:level 10
   :material "iron"
   :price (40 silver-coin)))

;;; Skills.

(defskill logging
  (:name "logging"
   :summary "Allows you to obtain logs by chopping down trees."
   :price (20 silver-coin)
   :required-tool logging-axe))

;;; Trainer.

(defentity logging-trainer (trainer)
  (:teaches logging))
