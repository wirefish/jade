(in-package :jade.isle-of-dawn)

(defentity isle-location ()
  (:domain :outdoor
   :surface :grass))

(defentity gravel-path ()
  (:brief "a gravel path"
   :description "The path's surface of finely crushed white stone contrasts with
   the lush greenery which surrounds you."))

(deflocation hilltop (isle-location)
  (:name "Hilltop"
   :description "You are standing atop a low hill in the center of a small
     island. A ring of ancient standing stones encircles the crown of the hill.
     The long grass that covers the hillside sways back and forth in the gentle
     breeze."

   :tutorial "Welcome to Atalea! As a new player, you will sometimes see green
     text like this. These messages provide tips to help you get started. For
     details, type `help tutorial`.

     A few basics: to move around the world, type the direction you want to go.
     For example, type `south` or `s` to move south. To look at your
     surroundings, type `look`. To look at something specific, add its name or
     description. For example, try `look standing stones` or `look self`.

     Head `south` to begin your adventure!"

   :contents nil
   :exits ((gravel-path :south pavilion))))
