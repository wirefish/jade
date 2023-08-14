(in-package :jade.lib)

;;; Races.

(defentity reborn-spirit ()
  (:brief "a reborn spirit"
   :icon 'ghost))

(defentity human ()
  (:brief "a human"
   :icon 'human))

(defentity elf ()
  (:brief "an el[f|ves]"
   :icon 'elf))

(defentity sidhe ()
  (:brief "a sidhe[]"
   :icon 'sidhe))

(defentity dwarf ()
  (:brief "a dwar[f|ves]"
   :icon 'dwarf))

(defentity goblin ()
  (:brief "a goblin"
   :icon 'goblin))

(defentity ogre ()
  (:brief "an ogre"
   :icon 'ogre))

;;; The prototype for every new avatar.

(defentity new-avatar (avatar)
  (:race reborn-spirit
   :attacks (fist foot)))
