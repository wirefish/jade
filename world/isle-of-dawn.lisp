(in-package :jade.isle-of-dawn)

(defentity isle-location (location)
  (:domain :outdoor
   :surface :grass))

(defentity gravel-path ()
  (:brief "a gravel path"
   :description "The path's surface of finely crushed white stone contrasts with
     the lush greenery which surrounds you."))

;;; hilltop

(defentity standing-stone ()
  (:brief "a standing stone"
   :description "You count nine stones, each about 12 feet tall and four feet
     wide. The faint remains of ornate tracery are barely visible upon their
     weathered surfaces."
   :implicit t))

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

   :contents (standing-stone)
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

;;; wildflower-field

(defquest choose-a-race
  (:name "Let's Get Physical"
   :summary "Choose your physical form by meditating at one of the racial
     shrines on the Isle of Dawn, then return to the officious kobold.")

  (meditate
      :summary "Meditate at one of the racial shrines on the Isle of Dawn.")

  (done
      :summary "Talk to the officious kobold in the wildflower field."))

(defentity officious-kobold ()
  (:brief "an officious kobold"
   :pose "sites at a low table in the shade of a large umbrella."
   :description "The kobold is a tiny humanoid with reptilian features, sparse
     wiry hair, and knobbly gray skin. It is, however, impeccably groomed."
   :offers-quests (choose-a-race))

  (:when-talk ((actor &quest choose-a-race :available) self topic)
    (tell self actor "Hello, friend! As you may have noticed, your current body
      is just a ghostly manifestation of your spirit. To fix that you'll need to
      select a race and take on a physical form. It just so happens I can help
      you do exactly that!")
    (offer-quest self 'choose-a-race actor)
    (maybe-show-tutorial
     actor 'offer-quest
     "The kobold is offering you a quest! Quests are tasks set for you by the
     denizens of the world. Completing them can provide you with many kinds of
     rewards. Type `help quests` for more information."))

  (:after-accept-quest (actor (quest &quest choose-a-race) npc)
    (tell self actor "Excellent! To the west you will find several shrines, each
      dedicated to a different race. Talk to the caretaker at each shrine to
      learn more about his or her people.

      Then, when you find the race that's right for you, return to the selected
      shrine and `meditate`. The caretaker will, ahem, take care of the rest.

      Return to me once you have completed this task.")
    (maybe-show-tutorial actor 'accept-quest "You can use the `quest` command to
      see the quests you've accepted and track your progress toward their
      completion."))

  (:when-talk ((actor &quest choose-a-race meditate) self topic)
    (tell self actor "Have you selected a race yet? The caretakers to the west
      will be more than happy to describe their races and help you make the
      right decision."))

  (:when-talk ((actor &quest choose-a-race done) self topic)
    (tell self actor "A fine choice! I had no doubt you would choose to become
      ~a. I am something of an expert in these matters, after all."
          (describe-brief (? actor :race)))

    (advance-quest actor 'choose-a-race)

    (tell self actor
          "I can't help but notice that your new body, while quite lovely, is also quite
          naked. Aren't you cold? To the south you will find my friend Dhalia,
          the seamstress; talk to her and she will make sure you go forth in
          style.")))

(defentity low-table ()
  (:brief "a low table"
   :full "The table looks sturdy but is otherwise unremarkable."
   :implicit t))

(defentity umbrella ()
  (:brief "an umbrella"
   :full "The umbrella is decorated with green and yellow stripes."
   :implicit t))

(deflocation wildflower-field (isle-location)
  (:name "Field of Wildflowers"
   :description "Flowers of every color and description bloom in the expansive
     fields along the sides of the path. The air is heavy with their fragrance."
   :tutorial "The ! symbol on the map means that a creature wants to talk to you
     about an available quest. You can `talk` to the creature to learn more.
     When you see the &mldr; symbol, you can `talk` to the creature about a
     quest you have accepted but not yet completed. If you see the &#x2713;
     symbol, then you have completed a quest and can `talk` to the creature to
     receive your rewards!

     The kobold has a quest for you. Try typing `talk kobold`."
   :contents (officious-kobold low-table umbrella)
   :exits ((gravel-path :north pavilion :south clothing-stall
                        :west human-shrine))))
