(in-package :jade.lib)

;;; Races.

(defentity reborn-spirit ()
  (:brief "a reborn spirit"
   :icon 'ghost))

(defentity human ()
  (:brief "a human"
   :icon 'human))

;;; The prototype for every new avatar.

(defentity new-avatar (avatar)
  (:race reborn-spirit
   :level 0
   :xp 0))
