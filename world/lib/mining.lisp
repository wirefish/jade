(in-package :jade.lib)

;;; Ore.

(defentity ore (item)
  (:brief "a chunk[s] of ~*~a ore"
   :stackable t))

(defentity copper-ore (ore)
  (:material "copper"))

(defentity tin-ore (ore)
  (:material "tin"))

(defentity zinc-ore (ore)
  (:material "zinc"))

(defentity silver-ore (ore)
  (:material "silver"))

(defentity gold-ore (ore)
  (:material "gold"))

(defentity iron-ore (ore)
  (:material "iron"))

;;; TODO: Other mineable things.

;;; TODO: Ore deposits.
