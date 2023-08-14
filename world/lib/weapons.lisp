(in-package :jade.lib)

(defentity weapon (item)
  (:icon 'staff
   :speed 3
   :damage-type :crushing
   :damage-range (1 4)
   :attack-verb "hits"))

(defentity one-handed-weapon (weapon)
  (:equippable-slot :main-hand))

(defentity two-handed-weapon (weapon)
  (:equippable-slot :both-hands))

;;; A natural weapon inherits its user's level.

(defentity natural-weapon (weapon)
  (:level nil))

(defentity fist (natural-weapon)
  (:brief nil
   :speed 2
   :attack-verb "punches"))

(defentity foot (natural-weapon)
  (:brief nil
   :speed 2
   :attack-verb "kicks"))

;;; Daggers.

(defentity dagger (one-handed-weapon)
  (:brief "a ~a dagger"
   :description "The dagger has a double-edged ~a blade with a sharp point. Its ~a
     handle is wrapped with ~a."
   :icon :knives-01
   :speed 2
   :damage-type :piercing
   :damage-range (4 12)
   :attack-verb "pokes"))
