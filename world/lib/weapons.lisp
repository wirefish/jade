(in-package :jade.lib)

(defentity weapon (item)
  (:icon staff
   :speed 3
   :base-damage 2
   :damage-type :crushing
   :damage-variance 0.25
   :attack-verb "hits"
   :item-group :weapons))

;;; A natural weapon inherits its user's level. A value of nil for :brief can be
;;; used to avoid messages like "Ann kicks Bob with a foot".

(defentity natural-weapon (weapon)
  (:level nil))

(defentity fist (natural-weapon)
  (:brief nil
   :speed 2
   :damage-type :crushing
   :attack-verb "punches"))

(defentity foot (natural-weapon)
  (:brief nil
   :speed 2
   :damage-type :crushing
   :attack-verb "kicks"))

(defentity bite (natural-weapon)
  (:brief nil
   :speed 2
   :damage-type :piercing
   :attack-verb "bites"))

;;; Equippable weapons fall into one of three categories: light, one-handed, or
;;; two-handed.

(defentity light-weapon (weapon)
  (:base-damage 3
   :equippable-slot :either-hand))

(defentity one-handed-weapon (weapon)
  (:base-damage 4
   :equippable-slot :main-hand))

(defentity two-handed-weapon (weapon)
  (:base-damage 7
   :equippable-slot :both-hands))

;;; Daggers are light piercing weapons.

(defentity dagger (one-handed-weapon) ; FIXME: light-weapon
  (:brief "a ~a dagger"
   :description "The dagger has a double-edged ~a blade with a sharp point. Its ~a
     handle is wrapped with ~a."
   :icon knives-01
   :speed 2
   :damage-type :piercing
   :attack-verb "pokes"
   :proficiency 'dagger-proficiency
   :mastery 'dagger-mastery
   :nonproficiency-penalty 0.5))

;;; Wands are light weapons that do magical or elemental damage, depending on
;;; the specific wand.

(defentity wand (light-weapon)
  (:brief "a wand"
   :description "This light weapon appears to be a simple piece of polished
     wood, but it radiates a magical aura."
   :icon staves-01
   :speed 2.5
   :damage-type :arcane
   :attack-verb "zaps"
   :proficiency 'wand-proficiency
   :mastery 'dagger-mastery
   :nonproficiency-penalty 0.25))

;;; Maces are one-handed crushing weapons.

(defentity mace (one-handed-weapon)
  (:brief "a mace"
   :description "This weapon has a heavy metal ball attached to a stout wooden
     handle, sized for use in one hand."
   :icon bluntweapons-05
   :speed 3.5
   :damage-type :crushing
   :damage-variance 0.2
   :attack-verb "smashes"
   :proficiency 'mace-proficiency
   :mastery 'mace-mastery
   :nonproficiency-penalty 0.5))

;;; Mauls are two-handed crushing weapons.

(defentity maul (two-handed-weapon)
  (:brief "a maul"
   :description "This weapon has a heavy metal ball attached to a long wooden
     handle. It requires both hands to wield."
   :icon bluntweapons-05
   :speed 5
   :damage-type :crushing
   :damage-variance 0.2
   :attack-verb "smashes"
   :proficiency 'maul-proficiency
   :master 'maul-mastery
   :nonproficiency-penalty 0.25))

;;; Swords are one-handed slashing (or piercing) weapons.

(defentity sword (one-handed-weapon)
  (:brief "a sword"
   :description "This weapon has a long metal blade attached to a plain wooden
     hilt, sized for use in one hand."
   :icon swords-01
   :speed 3
   :damage-type :slashing
   :damage-variance 0.3
   :attack-verb "slashes"
   :proficiency 'sword-proficiency
   :mastery 'sword-mastery
   :nonproficiency-penalty 0.25))

;;; Greatswords are two-handed slashing weapons.

(defentity greatsword (two-handed-weapon)
  (:brief "a greatsword"
   :description "This weapon has a long, dual-edged blade and requires both
     hands to wield."
   :icon swords-01
   :speed 5
   :damage-type :slashing
   :damage-variance 0.3
   :attack-verb "slashes"
   :proficiency 'greatsword-proficiency
   :mastery 'greatsword-mastery
   :nonproficiency-penalty 0.25))

;;; Axes are one-handed slashing weapons.

(defentity axe (one-handed-weapon)
  (:brief "an axe"
   :description "This weapon has a curved blade attached to one end of a wooden
    shaft, sized for use in one hand."
   :icon axes-01
   :speed 3.5
   :damage-type :slashing
   :damage-variance 0.33
   :attack-verb "slashes"
   :proficiency 'axe-proficiency
   :mastery 'axe-mastery
   :nonproficiency-penalty 0.5))

;;; Battle-axes are two-handed slashing weapons.

(defentity battle-axe (two-handed-weapon)
  (:brief "a battle axe"
   :description "This weapon has a large blade attached to one end of a long
     wooden haft, and requires both hands to wield."
   :icon axes-01
   :speed 5
   :damage-type :slashing
   :damage-variance 0.33
   :attack-verb "slashes"
   :proficiency 'battle-axe-proficiency
   :mastery 'battle-axe-mastery
   :nonproficiency-penalty 0.25))

;;; Polearms are two-handed piercing or slashing weapons.

(defentity polearm (two-handed-weapon)
  (:brief "a polearm"
   :description "This weapon has a pointed blade attached to one end of a long
     wooden haft, and requires both hands to wield."
   :icon spears-01
   :speed 4.5
   :damage-type :piercing
   :attack-verb "pierces"
   :proficiency 'polearm-proficiency
   :mastery 'polearm-mastery
   :nonproficiency-penalty 0.25))

;;; Staves are two-handed magical or elemental weapons.

(defentity staff (two-handed-weapon)
  (:brief "a staff"
   :description "This two-handed weapon is a polished length of wood that
     radiates a strong magical aura."
   :icon staves-04
   :speed 4
   :damage-type :arcane
   :attack-verb "zaps"
   :proficiency 'staff-proficiency
   :mastery 'staff-mastery
   :nonproficiency-penalty 0.25))
