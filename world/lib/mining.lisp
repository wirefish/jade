(in-package :jade.lib)

;;; Ore.

(defentity ore (item)
  (:brief "chunk of ~a ore"
   :stackable t))

(defentity copper-ore (ore)
  (:brief "a chunk of copper ore"

;;; Ingots.

(defentity ingot (item)
  (:stackable t))

(defentity copper-ingot (ingot)
  (:brief "a copper ingot"
   :materials (copper)))

(defentity silver-ingot (ingot)
  (:brief "a copper ingot"
   :materials (copper)))

  (:brief "a ~a ingot"
   :stackable t))
