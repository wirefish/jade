(in-package :jade.lib)

;;; Define the sort ordering for the groups and subgroups used by item
;;; definitions.

(define-item-groups
  (weapon dagger mace maul sword greatsword axe battle-axe spear wand staff)
  (armor head torso back hands waits legs feet)
  (accessory ears neck wrist finger)
  (storage backpack belt-pouch)
  (tool botany logging mining skinning fishing)
  (resource botany logging mining skinning fishing)
  (miscellany)
  (currency)
  (quest-item))

(defentity quest-item (item)
  (:item-group (quest-item)))

;;; Define the available damage types. Each definition is a symbol used to
;;; denote the damage type follwed by its name and a verb that describes its
;;; effect. The corresponding resistance is automatically defined by appending
;;; -resistance to the key, e.g. cold -> cold-resistance.

(define-damage-types
  (crushing "crushing" "crushes")
  (slashing "slashing" "slashes")
  (piercing "piercing" "pierces")
  (fire "fire" "burns")
  (cold "cold" "freezes")
  (acid "acid" "erodes")
  (electricity "electricity" "zaps")
  (arcane "arcane power" "damages"))
