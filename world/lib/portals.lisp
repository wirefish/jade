(in-package :jade.lib)

;;;; Standard portals.

(defentity entry-doorway ()
  (:brief "a doorway"))

(defentity exit-doorway ()
  (:brief "a doorway"))

(defentity stairway ()
  (:brief "a stairway"))

(defentity gravel-path ()
  (:brief "a gravel path"
   :description "The path's surface of finely crushed white stone contrasts with
     the lush greenery which surrounds you."))

(defentity continuing-portal ()
  (:pose "continues to the ~(~a~)."
   :unmatchable t))
