(in-package :jade.lib)

(defentity currency (item)
  (:stackable t
   :size +miniscule+
   :item-group :currency))

(defentity silver-coin (currency)
  (:brief "a silver coin"
   :icon silver_coin))
