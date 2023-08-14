(in-package :jade.isle-of-dawn)

(defentity isle-of-dawn ()
  (:name "Isle of Dawn"
   :description "The souls of ancient heroes are reborn atop a sacred hill in
     the middle of this small island. The magical energies of the place allow a
     soul to regain its physical form so it may join the battle against the
     great evil that threatens Atalea."
   :climate :temperate
   :level-range '(0 1)))

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

(defentity spirit-warden (humanoid)
  (:brief "the spirit warden"
   :pose "stands nearby, smiling amiably."
   :description "The spirit warden is an elderly human man, standing well over
     six feet tall. His long white hair and wispy beard frame a wrinkled,
     grinning face. He wears dark blue robes with gold embroidered trim.")

  (:after-enter-location ((actor &quest choose-a-race :available) location entry)
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

(defentity officious-kobold (humanoid)
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

    (advance-quest self actor 'choose-a-race)

    (tell self actor "I can't help but notice that your new body, while quite lovely,
      is also quite naked. Aren't you cold? To the south you will find my friend
      Dhalia, the seamstress; talk to her and she will make sure you go forth in
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

;;; human-shrine

(defentity human-caretaker (humanoid)
  (:brief "the human caretaker"
   :pose "stands nearby with a welcoming expression."
   :description "The caretaker is a tall, athletic woman wearing a practical
     leather outfit and a broad-brimmed hat. Various gardening tools hang from
     her wide leather belt.")

  (:when-talk ((actor &race human) self topic)
    (tell self actor "Well hello, fellow human!"))

  (:when-talk (actor self topic)
    (tell self actor "Hello, traveler. I imagine you are curious about humans.
      Here is what I can tell you.

      First and foremost, humans are known for their optimism and their
      adaptability. We feel like we can do anything, and do it well. Perhaps
      you've heard the expression, \"Jack of all trades, master of none?\" Some
      might consider it a negative, but we humans take pride in our ability to
      do a little bit of everything.

      Second, humans are especially noted for their skill as gatherers and
      farmers. We are adept at using our ingenuity to extract the bounty of the
      earth.

      If you want to join humankind, `meditate` here. I will then use the power of
      the shrine to complete your transformation.")))

(deflocation human-shrine (isle-location)
  (:name "Shrine of Humanity"
   :description "This part of the isle is a large garden. Plants bearing
     flowers, fruits, and vegetables are arranged in orderly rows. The soil is
     dark and fertile."
   :contents (human-caretaker)
   :exits ((gravel-path :east wildflower-field :north elven-shrine
                        :west ogre-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditate))
    (show actor "A calming warmth suffuses your being. The caretaker smiles broadly as she
      reaches out to you with her open hand, holding it inches from your ghostly
      form. She smiles broadly as the warmth spreads to her hand.

      After a moment she closes her palm. You see a faint glow between her
      fingers which quickly grows brighter. When she opens her hand she holds a
      tiny seedling, its delicate leaves unfolding before your eyes.")
    (change-race actor 'human)
    (advance-quest self actor 'choose-a-race))

  (:after-meditate (actor)
    (show actor "The caretaker nods in approval.")))

;;; elven-shrine

(defentity wooden-sculpture ()
  (:brief "a wooden sculpture"
   :pose "stands in the middle of the ring of trees."
   :description "The sculpture is carved from polished yellow wood. Its form is
     fluid and abstract but somehow evokes images of towering forest oaks and
     hidden woodland dells. Golden-brown moss grows in chaotic yet precise
     patterns along its sides. Atop the sculpture rests a shallow bowl made of
     green glass. The bowl contains clear, cold water."))

(defentity elven-caretaker (humanoid)
  (:brief "the elven caretaker"
   :pose "kneels beside one of the trees, her eyes closed."
   :description "The elven caretaker is a graceful young woman with striking
     emerald eyes and long, silver hair arranged in complex braids. She wears a
     pale green dress with embroidered trim.")

  (:when-talk (actor self topic)
    (tell self actor "Greetings, stranger. Be welcome in this place. No doubt
      you have come here to learn something about my people. I will tell you
      what I can.

      Mine are a peaceful, thoughtful people who love and respect nature. This
      was not always so; millenia ago we nearly destroyed ourselves with our
      prideful, warlike ways. It took a terrible crisis for my ancestors to
      change. I pray that we never slide back into those habits that nearly left
      us extinct.

      Elves are quick-footed and quick-witted, although we are not as strong as
      many of the other races of Atalea. We excel as hunters, magicians, bards,
      and---it pains me to say---as thieves. We consider ourselves custodians of
      the forest and take that charge very seriously. We do not stand idly by if
      our beloved home is threatened.

      If you would join my folk, simply `meditate` here to make your purpose
      known; the shrine's power will do the rest.")))

(deflocation elven-shrine (isle-location)
  (:name "Shrine of the Forest"
   :description "A low wooden fence surrounds a perfect ring of thirteen
     graceful birch trees. The area radiates an aura of tranquility."
   :surface :forest
   :contents (wooden-sculpture elven-caretaker)
   :exits ((gravel-path :south human-shrine :north sidhe-shrine :west goblin-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditate))
    (show actor "The caretaker approaches the nearby sculpture and raises her
      hands to the sky. Motes of light rise from the bowl atop the sculpture;
      they quickly become so bright you are forced to look away. A swirling wind
      rises and the trees begin to sway, their leaves glimmering in the light.
      Soon the motes move toward you, surrounding you with their light and
      warmth.

      After a few moments the wind dies down and the motes dissolve into tiny
      sparkling flecks which quickly disperse in the dying breeze. The clearing
      is calm once again.")
    (change-race actor 'elf)
    (advance-quest self actor 'choose-a-race))

  (:after-meditate (actor)
    (show actor "The caretaker bows respectfully.")))

;;; sidhe-shrine

(defentity sidhe-caretaker (humanoid)
  (:brief "the sidhe caretaker"
   :pose "stands nearby, lost in thought."
   :description "The sidhe caretaker is a tall, gaunt man with a stern look. His
     long white hair is pulled back into a ponytail that nearly reaches the
     ground. He wears dark robes of a rather elaborate and archaic style.")

  (:when-talk (actor self topic)
    (tell self actor "Well, well. Another hero, reborn. I suppose I should tell
      you something of my race, although I find it unlikely you were fortunate
      enough to be a member of the Fair Folk in your past life.

      For uncounted millenia, my people lived in an alternate plane of existence
      called the Shadowlands. A few short centuries ago, war with demons drove
      us out of our homeland. We ended up here.

      At our core, we sidhe are creatures of magic. It is our very essence. Few
      sidhe have become famous warriors or minstrels, although of course it is
      possible. But all of our people have some small skill in magic, and many
      have become wizards of reknown.

      I cannot say it seems likely, but if you believe you are truly one of us
      then I encourage you to `meditate` before the shrine. If you are found
      worthy, I will welcome you into the fold.")))

(defentity tree ()
  (:brief "a tree"
   :description "The tree is clearly very old. You get a strange sense that it
     does not belong in this world."
   :implicit t))

(defentity boulder ()
  (:brief "a boulder"
   :description "Upon closer inspection, you notice that the entire surface of
     the boulder is covered with tiny rune-like markings."
   :implicit t))

(deflocation sidhe-shrine (isle-location)
  (:name "Shrine of Shadows"
   :description "A lone tree grows atop an enormous granite boulder. Its
     gnarled, gray roots wrap around the stone before sinking into the fertile
     earth below. The tree's twisted branches splay outward and its dense leaves
     form a solid canopy that condemns this part of the isle to perpetual
     shadow."
   :surface :forest
   :contents (sidhe-caretaker tree boulder)
   :exits ((gravel-path :south elven-shrine :west dwarven-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditate))
    (show actor "As you begin your meditation, the shadows in the area deepen.
      The leaves above begin to rustle, as if the tree is growing restless.

      You vision begins to blur. Shifting figures appear, ghosts of creatures
      from another world. They call to you in an alien language you have never
      heard.

      After a moment, you begin to understand their words. You answer their
      calls, but the meaning of your words is lost as soon as they are uttered.
      Apparently satisfied, the figures disappear; the shadows retreat.")
    (change-race actor 'sidhe)
    (advance-quest self actor 'choose-a-race))

  (:after-meditate (actor)
    (show actor "The caretaker arches an eyebrow.")))

;;; dwarven-shrine

(defentity dwarven-caretaker (humanoid)
  (:brief "the dwarven caretaker"
   :pose "stands proudly amid the statues."
   :description "The dwarven caretaker stands about four feet tall, with
     shoulders nearly as wide. He wears full chain mail, even in the heat of the
     day. His armor appears to be of fine workmanship. A heavy maul hangs from
     his belt. His long, red beard falls across a belly that has seen a few
     pints of ale in its day.")

  (:when-talk (actor self topic)
    (tell self actor "Well met! If it's a dwarf ye wanna be, then ye be at tha
      right place. I kinna imagine wantin' ta be anythin' else!

      Me people may be wee, but we have tha strength of tha mountains within us!
      Aye, it's true, we have a great love for gold and gems. But we have nae
      great love for tha dark beasties that lurk below tha earth. We dwarves are
      great crafters and even greater at fightin', when tha times require it.

      If ye think that all sounds good, then get ta meditatin'!")))

(deflocation dwarven-shrine (isle-location)
  (:name "Shrine of the Mountain"
   :description "Rows of stone statues stand in the tall grass, each depicting a
     stout warrior with a grim expression and glowing amber eyes. The statues
     are arrayed in formation like soldiers marching to battle."
   :contents (dwarven-caretaker)
   :exits ((gravel-path :east sidhe-shrine :south goblin-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditation))
    (show actor "The caretaker grasps his maul with both hands and holds it out
      before him. After a moment the hammer begins to vibrate and erupts with
      amber light, echoing the statues' eyes. The vibrations grow stronger and
      emanate outward from the caretaker's body. Soon the entire area is shaking
      and the statues begin to rock back and forth.

      A powerful voice intones, \"You have chosen. So be it. Strong as stone,
      bright as steel. May you bring honor to clan and king.\"

      Without warning the shaking stops and the light dissipates.")
    (change-race actor 'dwarf)
    (advance-quest self actor 'choose-a-race))

  (:after-meditate (actor)
    (show actor "The caretaker joins you in silent reflection.")))

;;; goblin-shrine

(defentity chessboard ()
  (:brief "a stone chessboard"
   :pose "rests atop a low table."
   :description "The chessboard is remarkable for its pieces: each is a finely
     detailed and garishly-painted rendition of a creature who looks much like
     the caretaker. The pieces are so realistic they seem almost alive."))

(defentity goblin-caretaker (humanoid)
  (:brief "the goblin caretaker"
   :pose "stands beside the chessboard, apparently pondering his next move."
   :description "The caretaker is a short, big-eared creature with greenish-blue
     skin. In contrast with his somewhat comical proportions and garish attire,
     his large dark eyes evince a keen intellect.")

  (:when-talk (actor self topic)
    (tell self actor "Hello, friend! I can't tell you how happy I am to see your
      interest in goblinkind. We are a misunderstood folk! Let me set the record
      straight.

      Goblins are small in stature but we have big dreams. Mostly dreams of
      wealth, it's true, but there's nothing wrong with a little spending cash,
      eh?

      Some larger folks will tell you we are thieves, beggars, and worse. That
      is so mostly untrue! The majority of us would rather get rich through
      shrewd business practices and hard bargaining than by using less savory
      methods.

      When those ignorant louts take offense at our methods, it certainly helps
      that we're quick and stealthy by nature. You won't see many goblins waving
      huge swords around, but as they say, \"Size isn't everything!\"

      If you feel like you're one of us at heart, why not become one of us in
      body, too? Just `meditate` here and the deed will be done.")))

(deflocation goblin-shrine (isle-location)
  (:name "Shrine of Fortune"
   :description "The ground here has been cleared, leveled, and surfaced with
     multi-colored bricks."
   :surface :stone
   :contents (chessboard goblin-caretaker)
   :exits ((gravel-path :north dwarven-shrine :south ogre-shrine
                        :east elven-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditate))
    (show actor "As soon as you close your eyes, you have a vision of the world
      around you getting larger...or are you getting smaller? For a moment you
      are stricken with vertigo as the world shifts and spins.

      Once the dizziness passes, you open your eyes to find that your stature
      and greenish skin now match those of the caretaker.")
    (change-race actor 'goblin)
    (advance-quest self actor 'choose-a-race)
    (tell self actor "Huzzah! Welcome to the family!"))

  (:after-meditate (actor)
    (show actor "The caretaker leaves you to your meditation.")))

;;; ogre-shrine

(defentity ogre-caretaker (humanoid)
  (:brief "the ogre caretaker"
   :pose "is here, casually pulverizing rocks with his hammer."
   :description "The caretaker is a hulking figure, standing over eight feel
     tall. He wears an iron helm, iron gauntlets, and a breechclout. He carries
     an enormous iron sledgehammer.")

  (:when-talk (actor self topic)
    (tell self actor "Ugh. I be ogre of few words, but I know what you want. I
      tell you of my kin. We big. We strong. Done!

      You came to `meditate`? Ha. If you wanna be ogre, do it. Done!")))

(deflocation ogre-shrine (isle-location)
  (:name "Shrine of Strength"
   :description "This area is choked with a haphazard collection of rough stone
     blocks, many overgrown with vines and twisting grasses. The blocks'
     surfaces have been crudely painted with scenes of carnage featuring
     enormous warriors laying waste to their tiny assailants."
   :surface :stone
   :contents (ogre-caretaker)
   :exits ((gravel-path :north goblin-shrine :east human-shrine)))

  (:after-meditate ((actor &quest choose-a-race meditate))
    (show actor "The caretaker selects a boulder and tosses it on the ground
      before you. He then steps over, raises his hammer, and crushes the rock as
      he lets out an enormous belly laugh.

      Rocks fly around. Muscles grow. Brain shrink.")
    (change-race actor 'ogre)
    (advance-quest self actor 'choose-a-race))

  (:after-meditate (actor)
    (show actor "The caretaker chuckles and crushes another boulder.")))

;;; clothing-stall

(defquest get-some-clothes
  (:name "Cover Up"
   :summary "Pick a white tulip for Dhalia to exchange for a set of clothes."
   :required-quests (choose-a-race))

  (active
      :summary "Pick a white tulip.")

  (done
      :summary "Give the white tulip to Dhalia."))

(defentity seamstress (humanoid)
  (:name "Dhalia"
   :pose "stands in the stall, organizing her wares."
   :description "Dhalia is a human woman of indeterminate age. She wears
     silver-rimmed spectacles and an impeccably-tailored dress decorated with a
     floral pattern."
   :offers-quests (get-some-clothes))

  (:when-talk ((actor &quest get-some-clothes :available) self topic)
    (tell self actor "Greetings! Did our mutual kobold friend send you my way? It
      certainly seems you have need of my wares. I will happily provide you with
      an outfit that should serve your needs, but I must ask a favor in
      return.")
    (offer-quest self 'get-some-clothes actor))

  (:after-accept-quest (actor (quest &quest get-some-clothes) self)
    (tell self actor "Wonderful! You see, I am very fond of white tulips, but my
      work here prevents me from taking the time to gather them. Would you head
      to the east and get one for me?"))

  (:when-talk ((actor &quest get-some-clothes active) self topic)
    (tell self actor "Have you found a white tulip for me? They're just to the
      east."))

  (:when-talk ((actor &quest get-some-clothes done) self topic)
    (tell self actor "What is that you have in your hand?")
    (advance-quest self actor 'get-some-clothes))

  (:after-finish-quest (actor (quest &quest get-some-clothes))
    (tell self actor "Why thank you, this tulip is lovely! Your timing is
      perfect; I have just finished selecting an outfit for you. I hope
      everything fits!")
    (give self actor
          (list (clone-entity 'shirt :materials '(cotton))
                (clone-entity 'pants :materials '(cotton))
                (clone-entity 'shoes :materials '(worn-leather))
                (clone-entity 'small-backpack :materials '(canvas))))
    (maybe-show-tutorial actor 'items "The seamstress have given you several
      items; type `inventory` or `inv` to list them.

      To wear an item, use the `equip` command. For example, type `equip shirt`.
      Type just `equip` to list the items you currently have equipped.

      In addition to clothing, the seamstress also gave you a backpack.
      Equipping it will increase the number of items you can carry before you
      become encumbered.

      For more information type `help equip` or `help inventory`.")))

(deflocation clothing-stall (isle-location)
  (:name "Clothing Stall"
   :description "A wooden market stall has been erected beside the path. Its
     counter is piled with basic clothing items in myriad styles and sizes."
   :contents (seamstress)
   :exits ((gravel-path :north wildflower-field :south circle-of-names
                        :east tulip-field-sw))))

;;; tulip-field

(defentity white-tulip (item)
  (:brief "a white tulip"
   :pose "draws your attention."
   :description "The tulip is quite lovely; you can see why Dhalia prizes them."
   :entry-message "catches your eye."
   :quest get-some-clothes)

  (:allow-take ((actor &quest get-some-clothes active) self container))

  (:allow-take (actor self container)
    (show actor "It would be rude to pick the tulip right now.")
    (disallow-action))

  (:after-take (actor self container)
    (with-delay (15)
      nil)
    (advance-quest self actor 'get-some-clothes)))

(defentity tulip-field-portal ()
  (:brief "the tulip field"
   :pose "continues to the ~(~a~)."
   :unmatchable t))

(defentity tulip-field (isle-location)
  (:name "Field of Tulips"
   :description "Tulips in myriad colors have been planted here."
   :surface :flowers)

  (:after-enter-world ()
    (spawn-unique-entity self 'white-tulip)))

(deflocation tulip-field-sw (tulip-field)
  (:tutorial "Some items can be picked up using the `take` command. For example,
    type `take tulip` to take a white tulip when you see one. Picking up an item
    places it into your inventory. Use the `inventory` command (or just `inv`
    for short) to list the items you are carrying."
   :exits ((gravel-path :west clothing-stall)
           (tulip-field-portal :east tulip-field-se :north tulip-field-nw))))

(deflocation tulip-field-se (tulip-field)
  (:exits ((tulip-field-portal :west tulip-field-sw :north tulip-field-ne))))

(deflocation tulip-field-nw (tulip-field)
  (:exits ((tulip-field-portal :south tulip-field-sw :east tulip-field-ne))))

(deflocation tulip-field-ne (tulip-field)
  (:exits ((tulip-field-portal :west tulip-field-nw :south tulip-field-se))))

;;; circle-of-names

(defentity tree-of-names ()
  (:brief "a beautiful tree"
   :pose "grows in the center of the lawn."
   :description "The tree has smooth, silvery bark and broad leaves of the
     deepest green. Thousands of names have been written in luminous ink upon
     its trunk, all using the same flowing script."))

(defquest choose-a-name
  (:name "Nameless No More"
   :summary "Choose your name by saying it in the presence of the orb of naming,
     then return to the mistress of names."
   :required-quests (get-some-clothes))

  (active
      :summary "Speak your name in the presence of the orb of naming.")

  (done
      :summary "Return to the mistress of names."))

(defentity mistress-of-names (humanoid)
  (:brief "the mistress of names"
   :pose "stands beneath the tree."
   :description "The mistress of names is a short, slender woman of
     indeterminate age. Her long auburn hair is bound in a loose ponytail. She
     wears a pair of horn-rimmed spectacles and her clothing is rumpled and
     ink-stained."
   :offers-quests (choose-a-name))

  (:when-talk ((actor &quest choose-a-name :finished) self topic)
    (tell self actor "Ah, ~a. I just love the sound of your name! It simply
      rolls off the tongue. My name, you ask? I don't have one."
          (? actor :name)))

  (:when-talk ((actor &quest choose-a-name :unavailable) self topic)
    (show actor "The mistress of names frowns at you over her glasses."))

  (:when-talk ((actor &quest choose-a-name :available) self topic)
    (tell self actor "Oh, hello. I didn't see you standing there. I hope you
      haven't been waiting long. How can I help you?

      Ah! You need a name, don't you. You've come to the right place; I'd be
      happy to help you out.")
    (offer-quest self 'choose-a-name actor))

  (:after-accept-quest (actor (quest &quest choose-a-name) self)
    (tell self actor "Just to the east you'll see a magical orb. Find it and say
      the word. Literally! Stand next to the orb and `say` the word you want to
      have as your name. The orb's power is truly remarkable.

      Once your anonymity has been cured, come back to me so I can record your
      new name on the trunk of this tree with the names of all the other heroes
      who have passed this way."))

  (:when-talk ((actor &quest choose-a-name active) self topic)
    (tell self actor "Still going incognito? You'll find the orb of naming just
      to the east."))

  (:when-talk ((actor &quest choose-a-name done) self topic)
    (tell self actor "Yes, what is it? Of course, you've chosen your name! Let me see, where is my
      pen? Ah, there it is. And now to write your name --- how did you spell it,
      again? Just a few strokes of the pen, and... done!")
    (advance-quest self actor 'choose-a-name)
    (tell self actor "~a is a fine name. Wear it proudly." (? actor :name))))

(deflocation circle-of-names (isle-location)
  (:name "Circle of Names"
   :description "You stand within a wide circle of well-tended lawn surrounded
     by a low stone wall."
   :contents (tree-of-names mistress-of-names)
   :exits ((gravel-path :north clothing-stall :east clifftop :south guard-station))))

;;; clifftop

(defentity orb-of-naming ()
  (:brief "the orb of naming"
   :pose "hovers a few feet above the ground."
   :description "The orb is a spherical stone about two feet in diameter. Its
     surface is smooth and cloudy. If a creature speaks a word within its
     presence, the orb has the power to make that word the creature's name.")

  (:after-say ((actor &quest choose-a-name active) message)
    (show actor "The orb begins to glow, dimly at first, but then more brightly.
      Sparks skitter across its smooth surface and you feel an uncomfortable
      tingle beneath your skin.")
    (if (change-name actor message)
        (advance-quest self actor 'choose-a-name)
        (show actor "The sparks subside and the orb's glow dims; nothing seems
           to happen."))))

(deflocation clifftop (isle-location)
  (:name "Windy Clifftop"
   :description "You stand atop a rocky cliff that falls perhaps a hundred feet
     to a narrow beach. The wind is brisk and smells of the sea."
   :tutorial "To speak, use the `say` command. For example, to say \"hello\",
     you could type `say hello`. Everyone (and everything!) in your location
     will hear what you say."
   :surface :stone
   :contents (orb-of-naming)
   :exits ((gravel-path :west circle-of-names)))

  (:after-enter-location ((actor &quest choose-a-name active) location entry)
    (maybe-show-tutorial actor 'orb-of-naming "The `look:orb of naming` is
      listening, so be careful what you say! If you speak a single word that the
      orb deems suitable, that word will become your name. Choose wisely.")))

;;; guard-station

(defquest kill-some-plants
  (:name "Weed Control"
   :summary "Prove your worth to the guard by killing a vineling. Lashleaf?
     Whatever."
   :required-quests (choose-a-name))

  (active
      :summary "Kill a vineling. Lashleaf? Whatever.")

  (done
      :summary "Return to the guard."))

(defentity copper-dirk (dagger)
  (:brief "a pitted copper dirk"
   :description "The dirk's blade is so worn that it seems likely to snap at any
     time."
   :materials (copper)))

(defentity guard (humanoid)
  (:brief "a burly guard"
   :pose "stands nearby."
   :description "The guard wears a long chainmail shirt and carries a
     double-bladed axe. His bristly red beard spills out across his ample
     belly."
   :offers-quests (kill-some-plants))

  (:when-talk ((actor &quest kill-some-plants :finished) self topic)
    (tell self actor "Good to see you again, plant-slayer."))

  (:when-talk ((actor &quest kill-some-plants :available) self topic)
    (tell self actor "Greetings, ~a. It seems you're nearly ready to leave this
      place, but I have my doubts. Before I allow you to venture further, I'm
      going to teach you how to fight! You'll need to be able to handle a weapon
      if you want to survive in the real world." (? actor :name))
    (offer-quest self 'kill-some-plants actor))

  (:after-accept-quest (actor (quest &quest kill-some-plants) self)
    (tell self actor "West of here you'll find some...plants. Not normal
      plants, but vicious killers! Vinelings, I think they're called. Or maybe
      lashleaves? Whatever. The name's not important. Here, take this.")

    (give self actor (list (clone-entity 'copper-dirk)))

    (tell self actor "Go ahead, `equip` that knife and kill one of those plants.
      Strike fast and true! If you can overcome such a fearsome foe, I'll
      happily let you pass."))

  (:when-talk ((actor &quest kill-some-plants active) self topic)
    (tell self actor "Any progress so far? Kill one of those plant things and
      we'll talk."))

  (:when-talk ((actor &quest kill-some-plants done) self topic)
    (tell self actor "Great job! I'll confess, those things give me the heebie
      jeebies. Plants shouldn't writhe around like that. Please, feel free to
      keep the dirk. You may head south whenever you like.")
    (advance-quest self actor 'kill-some-plants))

  (:allow-exit-location ((actor &quest kill-some-plants :finished) location (exit &dir :south))
    (show actor "The guard salutes as you head toward the gate."))

  (:allow-exit-location ((actor avatar) location (exit &dir :south))
    (show actor "The guard refuses to let you go that way.")
    (tell self actor "Stop right there, friend. You need to perform the tasks
      set for you by myself and my comrades to the north before I will let you
      pass.")
    (disallow-action)))

(defentity iron-gate ()
  (:brief "an iron gate"
   :transit-message "The gate swings shut silently behind you."))

(deflocation guard-station (isle-location)
  (:name "Guard Station"
   :description "A small guard post stands alongside the path."
   :tutorial "In your adventures you will have the chance to learn many
     different `help:skills`. Each skill allows you to perform a certain type of
     action, such as fighting with weapons, casting magic spells, or crafting
     items. The more you use a skill, the more your rank in that skill will
     increase. A higher rank allows you to succeed at more difficult tasks.

     The guard here has a quest that will teach you your first weapon skill.
     Talk to him to get started. You can also type `help skills` to learn more
     about skills in general."
   :contents (guard)
   :exits ((gravel-path :north circle-of-names :west overgrown-field-se)
           (iron-gate :south cobbled-square))))

;;; overgrown-field

(defentity lashling-tendril (natural-weapon)
  (:brief "a thorny tendril"
   :speed 5
   :damage-type :slashing
   :damage-range (2 6)
   :attack-verb "whips"))

(defentity lashling (combatant)
  (:brief "a lashling"
   :pose "flails its tendrils in a menacing display."
   :description "The lashling is a small mass of writhing vines and weeds that
     has somehow gained the ability to move, albeit very slowly. Sharp thorns
     protrude from the ends of its leafy, tentacle-like appendages."
   :icon 'pink_creature
   :entry-message "~a emerges from beneath the weeds."
   :attacks (lashling-tendril)
   :traits (:defense 0.5))

  (:after-kill ((actor &quest kill-some-plants active) self)
    (advance-quest self actor 'kill-some-plants)))

(defentity overgrown-field-portal ()
  (:brief "the field"
   :pose "continues to the ~(~a~)."
   :unmatchable t))

(defentity overgrown-field (isle-location)
  (:name "Overgrown Field"
   :description "Tangled vines and weeds make it difficult to move through this
     area."
   :surface :weeds)

  (:after-enter-world ()
    (spawn-unique-entity self 'lashling))

  (:after-kill (actor (target lashling))
    (with-delay (15)
      (spawn-unique-entity self 'lashling))))

(deflocation overgrown-field-sw (overgrown-field)
  (:exits ((overgrown-field-portal :east overgrown-field-se :north overgrown-field-nw))))

(deflocation overgrown-field-se (overgrown-field)
  (:tutorial "To enter combat with a lashling, type `attack lashling`. You will
     automatically perform basic attacks with the weapon in your main hand.
     Combat ends when you or your opponent is dead!

     Depending on the skills you choose to learn as you explore the world, you
     will gain access to a variety of special attacks and defenses that you can
     use during combat. Type `help combat` to learn more."
   :exits ((overgrown-field-portal :west overgrown-field-sw :north overgrown-field-ne)
           (gravel-path :east guard-station))))

(deflocation overgrown-field-nw (overgrown-field)
  (:exits ((overgrown-field-portal :south overgrown-field-sw :east overgrown-field-ne))))

(deflocation overgrown-field-ne (overgrown-field)
  (:exits ((overgrown-field-portal :west overgrown-field-nw :south overgrown-field-se))))

;;; cobbled-square

(defentity sandy-path ()
  (:brief "a sandy path"))

(defentity cobbled-lane ()
  (:brief "a cobbled lane"))

(deflocation cobbled-square (isle-location)
  (:name "Cobbled Square"
   :description "The fresh smell of the sea pleasantly fills this small seaside
     plaza."
   :tutorial "Completing quests has given you enough experience to gain a level!
     By doing so you have also gained `help:karma`, which you can use to join
     `help:guilds` and learn `help:skills`. Skills grant you access to special
     actions. As you travel the world, be on the lookout for trainers who can
     induct you into their guilds and teach you a variety of skills."
   :surface :stone
   :exits ((iron-gate :north guard-station) (sandy-path :west beach-east)
           (cobbled-lane :east pier) (entry-doorway :south dockmaster-shack))))

;;; beach

(defentity shiny-seashell (item)
  (:brief "a shiny seashell"
   :description "The seashell's polished surface is covered with an intricate
     pattern of white and orange whorls."
   :alts ("a shiny shell")
   :stackable t))

(defentity beach-portal ()
  (:brief "the beach"
   :pose "continues to the ~(~a~)."
   :unmatchable t))

(defentity beach-location (isle-location)
  (:name "Rocky Beach"
   :description "The sand on this narrow beach is full of pebbles and shell
     fragments."
   :surface :sand))

(deflocation beach-east (beach-location)
  (:exits ((beach-portal :west beach-center) (sandy-path :east cobbled-square))))

(deflocation beach-center (beach-location)
  (:exits ((beach-portal :west beach-west :east beach-east))))

(deflocation beach-west (beach-location)
  (:exits ((beach-portal :east beach-center)))

  (:after-enter-world ()
    (spawn-unique-entity self 'shiny-seashell))

  (:after-take (actor (item shiny-seashell) self)
    (with-delay ((uniform-random 200 500))
      (spawn-unique-entity self 'shiny-seashell))))

;;; dockmaster-shack

(defquest special-delivery
  (:name "On to Arwyck"
   :summary "Deliver the dockmaster's documents to the surly stevedore at the
     Arwyck docks."
   :required-quests (kill-some-plants))

  (active
      :summary "Board the *Siren* and arrive at the Arwyck docks.")

  (done
      :summary "Talk to the surly stevedore."))

(defentity bundle-of-documents (item)
  (:brief "a bundle[s] of documents"
   :description "The documents are rather mundane shipping records. A scrawled
     note on top reads, \"Pay the messenger 10 silver. Q.M.\""
   :quest special-delivery))

(defentity dockmaster (humanoid)
  (:brief "the dockmaster"
   :pose "sits behind the desk."
   :description "The dockmaster is a grizzled man with a short salt-and-pepper
     beard. His left eye is covered with a leather patch, but his right eye
     harbors a dangerous gleam."
   :offers-quests (special-delivery))

  (:when-talk ((actor &quest special-delivery :available) self topic)
    (tell self actor "You there. I've an errand that needs doing if you've a
      mind to earn some coin.")
    (offer-quest self 'special-delivery actor))

  (:after-accept-quest (actor (quest &quest special-delivery) self)
    (tell self actor "Good, good. Take these.")
    (give self actor (list (clone-entity 'bundle-of-documents)))
    (tell self actor "I need you to deliver those documents to my man in Arwyck.
      Surly fellow near the docks. Can't miss him. He'll pay you once he has the
      documents."))

  (:when-talk ((actor &quest special-delivery active) self topic)
    (tell self actor "What are you doing here? My man needs those documents. The
      *Siren* departs for Arwyck from the dock just east of here."))

  (:when-talk ((actor &quest special-delivery :finished) self topic)
    (tell self actor "Thanks for your help with the documents. You never know, I
      might have need for your services again.")))

#| FIXME: move to guy in arwyck
  (:when-talk ((actor &quest special-delivery done) self topic)
    (tell talk actor "Hey, thanks. I've been needin' those. Here's your coin!
      You'll be glad of it if you'll be visitin' the shops here in Arwyck. It's
      no bleedin' city, but you can find most of what you'll be needin' here in
      town.")
    ;; FIXME: (receive actor (make-entity 'silver-coin :quantity 10) npc)
  nil))
|#

(defentity desktop-documents ()
  (:brief "a document"
   :description "A discreet scan the papers on top of the pile show them to be
     schedules, manifests, and other such shipping-related documents."
   :implicit t))

(defentity wine-bottles ()
  (:brief "an empty wine bottle"
   :description "You may have been dead for hundreds of years, but wine hasn't
     changed all that much; you still recognize the cheap stuff when you smell
     it."
   :implicit t))

(deflocation dockmaster-shack (isle-location)
  (:name "Dockmaster's Shack"
   :description "This one-room structure is dominated by a large desk that is
     piled with documents and empty wine bottles."
   :domain :indoor
   :surface :wood
   :contents (dockmaster desktop-documents wine-bottles)
   :exits ((exit-doorway :north cobbled-square) (stairway :down dockmaster-basement))))

;;; dockmaster-basement

(deflocation dockmaster-basement (isle-location)
  (:name "Basement of the Dockmaster's Shack"
   :description "This low, damp space is more of a crawlspace than a basement.
     Several barrels and crates have been pushed into one corner of the dirt
     floor."
   :domain :indoor
   :surface :dirt
   :exits ((stairway :up dockmaster-shack))))

;;; pier

(defentity dock-sign ()
  (:brief "an informative sign"
   :pose "is posted here."
   :description "The sign indicates that a ship called the *Siren* frequently
     arrives here to carry passengers across the sea to the town of Arwyck."))

(deflocation pier (isle-location)
  (:name "Sturdy Pier"
   :description "This stone pier juts out into the sea, giving ships that visit
     the isle a safe place to dock."
   :tutorial "To board a ship for Arwyck, wait until it arrives and then move
     `east`. Once the ship reaches its destination, move `south` to disembark."
   :surface :stone
   :contents (dock-sign)
   :exits ((cobbled-lane :west cobbled-square))))

;;; the-siren

(defentity gangplank ()
  (:brief "a gangplank"))

(defentity sailor (humanoid)
  (:brief "a grizzled sailor"
   :description "The sailor has a short, salt-and-pepper beard and wears a faded
     uniform.")

  (:when-talk (actor self topic)
    (tell self actor "Welcome aboard the *Siren*! Every few moments we make the
      short run between Arwyck and the Isle of Dawn. Relax and enjoy the trip!")))

(deflocation the-siren (moving-location)
  (:name "The *Siren*"
   :description "This sturdy single-masted vessel makes frequent trips between the Isle
     of Dawn and the town of Arwyck on the mainland."
   :domain :outdoor
   :surface :wood
   :surround :water
   :icon 'boat
   :contents (sailor)

   :route-exits '((gangplank :west pier))))
