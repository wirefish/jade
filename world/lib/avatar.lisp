(in-package :jade.lib)

(defentity reborn-spirit ()
  (:brief "a reborn spirit"
   :icon 'ghost))

(defentity new-avatar (avatar)
  (:race reborn-spirit
   :level 0
   :xp 0))
