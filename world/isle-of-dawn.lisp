(in-package :jade.isle-of-dawn)

(defentity isle-location (location)
  (:domain :outdoor
   :surface :grass))

(defentity gravel-path ()
  (:brief "a gravel path"
   :description "The path's surface of finely crushed white stone contrasts with
     the lush greenery which surrounds you."))

;;; hilltop

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

;;; pavilion

(defentity spirit-warden ()
  (:brief "the spirit warden"
   :pose "stands nearby, smiling amiably."
   :description "The spirit warden is an elderly human man, standing well over
     six feet tall. His long white hair and wispy beard frame a wrinkled,
     grinning face. He wears dark blue robes with gold embroidered trim.")

  (:after-enter-location ((actor avatar) location entry)
    (show actor "The spirit warden beckons to you."))

  (:when-talk (actor (target self) topic)
    (tell self actor "Welcome to Atalea, hero! Or perhaps I should say
      \"welcome back...\"

      I don't know how to explain this, but you died long ago. My condolences.
      For reasons unknown, however, you have been recalled from the Dreamlands
      and once again walk in the physical world.

      My colleagues stand ready to help reorient you after your long absence. As
      you explore this isle, talk to anyone you meet; we are all here to help.

      To the south you will meet an odd creature who will get you started. Head
      that way when you are ready.")))

(deflocation pavilion (isle-location)
  (:name "Pavilion"
   :description "An open-air pavilion stands a few feet from the path. Its silk
     canopy is painted in bright colors."
   :tutorial "You will often encounter creatures with whom you can interact.
     They may provide useful information or offer rewards if you perform actions
     on their behalf. For example, type `talk warden` to talk to the spirit
     warden. He may have something interesting to say."
   :contents (spirit-warden)
   :exits ((gravel-path :north hilltop :south wildflower-field))))
