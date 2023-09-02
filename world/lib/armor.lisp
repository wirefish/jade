(in-package :jade.lib)

;;; Armor classifications trade off protection for speed.

(defentity light-armor (item)
  (:armor-multiplier 0.7
   :speed-multiplier 1.0))

(defentity medium-armor (item)
  (:armor-multiplier 1.0
   :speed-multiplier 1.2))

(defentity heavy-armor (item)
  (:armor-multiplier 1.3
   :speed-multiplier 1.4))

;;; Light armor provides the least protection but does not slow attacks.

(defentity light-helm (light-armor)
  (:brief "~a ~a helm"
   :description "The ~*~a helm provides basic protection to its wearer's head."
   :icon light-helm
   :equippable-slot :head
   :item-group (armor head)))

(defitems (light-helm helm)
  (1 "thin leather" 15)
  (5 "leather" 30)
  (10 "thick leather" 60))

(defentity light-brigandine (light-armor)
  (:brief "~a ~a brigandine"
   :description "The ~*~a brigandine provides basic protection to its wearer's
     torso."
   :icon light-brigandine
   :equippable-slot :torso
   :item-group (armor torso)))

(defitems (light-brigandine brigandine)
  (1 "thin leather" 25)
  (5 "leather" 50)
  (10 "thick leather" 100))

(defentity light-gloves (light-armor)
  (:brief "a pair of ~*~a gloves"
   :description "The ~*~a gloves provide basic protection to their wearer's
     hands."
   :icon light-gloves
   :equippable-slot :hands
   :item-group (armor hands)))

(defitems (light-gloves gloves)
  (1 "thin leather" 10)
  (5 "leather" 20)
  (10 "thick leather" 40))

(defentity light-leggings (light-armor)
  (:brief "a pair of ~*~a leggings"
   :description "The ~*~a leggings provide basic protection to their wearer's
     legs."
   :icon light-leggings
   :equippable-slot :legs
   :item-group (armor legs)))

(defitems (light-leggings leggings)
  (1 "thin leather" 20)
  (5 "leather" 40)
  (10 "thick leather" 80))

(defentity light-boots (light-armor)
  (:brief "a pair of ~*~a boots"
   :description "The ~*~a boots provide basic protection to their wearer's feet."
   :icon light-boots
   :equippable-slot :feet
   :item-group (armor feet)))

(defitems (light-boots boots)
  (1 "thin leather" 20)
  (5 "leather" 40)
  (10 "thick leather" 80))

;;; TODO: Medium armor provides moderate protection but slows attacks slightly.

;;; TODO: Heavy armor provides maximum protection but slows attacks significantly.
