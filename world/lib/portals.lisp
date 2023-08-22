(in-package :jade.lib)

;;;; Standard portals.

(defentity doorway ()
  (:brief "a doorway"))

(defentity entry-doorway ()
  (:brief "a doorway"
   :description "~a leads into a building."
   :exit-message "heads ~a through ~a."))

(defentity exit-doorway ()
  (:brief "a doorway"
   :description "~a leads out of the building."
   :exit-message "heads ~a through ~a."))

(defentity stairway ()
  (:brief "a stairway"))

(defentity gravel-path ()
  (:brief "a gravel path"
   :description "~a leads ~a. The path's surface of finely crushed white stone
     contrasts with the lush greenery which surrounds you."))

(defentity continuing-portal ()
  (:description "~a continues to the ~a."
   :look-article :definite
   :unmatchable t))
