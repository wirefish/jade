(in-package :jade.lib)

;;; Define the sort ordering for the groups and subgroups used by item
;;; definitions.

(define-item-groups
  (weapon dagger mace maul sword greatsword axe battle-axe spear wand staff)
  (armor head torso back hands waits legs feet)
  (accessory ears neck wrist finger)
  (storage backpack belt-pouch)
  (tool botany logging mining)
  (resource botany logging mining)
  (miscellany)
  (currency)
  (quest-item))

(defentity quest-item (item)
  (:item-group (quest-item)))
