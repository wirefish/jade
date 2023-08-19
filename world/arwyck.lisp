(in-package :jade.arwyck)

;;; Arwyck is the low-level hub town, and as such it hosts numerous vendors and
;;; trainers, along with a number of introductory quests to demonstrate
;;; different mechanics.
;;;
;;; In addition, some of the NPC dialogue starts to hint at the larger story
;;; arc, i.e. the disappearance of the gods and the troubles arising from that.
;;;
;;; Finally, the town has its own short quest arc involving a rash of thefts
;;; from various shops.

(defentity arwyck ()
  (:name "Village of Arwyck"
   :description "This once-sleepy hamlet has recently become a hub of
     adventuring activity."
   :climate :temperate
   :level-range (1 10)))

;;; common prototypes

(defentity rutted-path ()
  (:brief "a rutted path"))

(defentity dirt-road ()
  (:brief "a dirt road"))

(defentity narrow-path ()
  (:brief "a narrow path"))

(defentity alley ()
  (:brief "a narrow alley"))

(defentity cobbled-road ()
  (:brief "a cobbled road"))

(defentity guard (humanoid)
  (:brief "a guard"
   :description "The guard wears a chainmail hauberk and, over that, a tabard
     displaying the coat-of-arms of Arwyck: a golden sheave of wheat on a dark
     green field."))

;;; docks

(defentity dock-location (location)
  (:domain :outdoor
   :surface :wood
   :surround :shallow-water))

(defentity stevedore (humanoid)
  (:brief "a surly stevedore"
   :pose "stares out to sea."
   :description "The man wears stained overalls and a sour frown.")

  (:when-talk ((actor &quest jade.isle-of-dawn:special-delivery :done) self topic)
    (tell self actor "Hey, thanks. I've been needin' those. Here's your coin!
      You'll be glad of it if you'll be visitin' the shops here in Arwyck. It's
      no bleedin' city, but you can find most of what you'll be needin' here in
      town.")
    (receive actor self (list (clone-entity 'silver-coin :quantity 10)))
    (advance-quest self actor 'jade.isle-of-dawn:special-delivery))

  (:when-talk (actor self topic)
    (tell self actor "Ah, to be sailing the Emerald Sea. But here I am, stuck
      ashore, loading and unloading gods-know-what for gods-know-whom. Well,
      it's a living.")))

(deflocation west-dock (dock-location)
  (:name "West Dock"
   :description "A short wooden dock juts out into the bay."
   :contents (stevedore)
   :exits ((gravel-path :south bayside-plaza-2)))

  (:after-enter-location ((actor &quest jade.isle-of-dawn:special-delivery :active) self entry)
    (show actor "You've arrived in Arwyck!")
    (advance-quest self actor 'jade.isle-of-dawn:special-delivery)))

(deflocation east-dock (dock-location)
  (:brief "East Dock"
   :description "A rickety wooden dock extends out into the bay."
   :exits ((gravel-path :south bayside-plaza-4))))

;;; bayside plaza

(defentity plaza-portal (continuing-portal)
  (:brief  "the plaza"))

(defentity plaza-location (location)
  (:name "Bayside Plaza"
   :description  "A cobbled stone plaza runs along the shore of the bay."
   :domain :outdoor
   :surface :stone))

(defentity beggar (humanoid)
  (:brief "an old beggar"
   :pose "sits nearby."
   :description "The beggar looks like he's been dragged through a mile of mud.
     He smells worse than he looks.")

  (:when-talk (actor self topic)
    (show actor "The beggar looks up at you with milky white eyes and doesn't
      say a word. Despite his apparent blindness, he perfectly tracks your every
      move.")))

(deflocation bayside-plaza-1 (plaza-location)
  (:contents (beggar)
   :exits ((plaza-portal :east bayside-plaza-2))))

(deflocation bayside-plaza-2 (plaza-location)
  (:tutorial "Welcome to Arwyck! In this lively town you will find numerous vendors and
     trainers.

     Vendors can sell you many items, including armor and weapons, which will
     prove invaluable in your adventures. They are represented on the map by an
     icon that looks like a stack of coins. Use the `buy` command near a vendor
     to see what's for sale.

     Trainers are representatives of `help:guilds`. By joining a guild you gain
     access to certain `help:skills`, including those required to use any
     weapons you may buy.

     Trainers are represented on the map by an upward-pointing arrow icon. Use
     the `guild` command near a trainer to learn more."
   :exits ((plaza-portal :west bayside-plaza-1 :east bayside-plaza-3)
           (rutted-path :north west-dock))))

(defquest dance-monkey
  (:name "Dance Monkey"
   :summary "Find the monkey and dance for it, dance for it, dance for it, oh oh
     oh. Then tell the young laborer all about it. (You can dance using the
     `help:emote` command. For example, `emote dance`.)")
  (:active
      :summary "Dance in the presence of the monkey in Arwyck.")
  (:done
      :summary "Return to the young laborer and recount your dancing exploits."))

(defentity young-laborer (humanoid)
  (:brief "a young laborer"
   :pose "scrubs the cobbles nearby."
   :description "The laborer is a boy of perhaps fourteen summers. He is on his
     knees, scrubbing the cobbles with an old wire brush."
   :offers-quests (dance-monkey))

  (:when-talk ((actor &quest dance-monkey :available) self topic)
    (show actor "The boy looks up from his work.")
    (tell self actor "Oh my, I like your style! If you've got a moment, there's
      something that's been bothering me lately. I could really use your help.")
    (offer-quest self 'dance-monkey actor))

  (:after-accept-quest (actor (quest &quest dance-monkey) self)
    (tell self actor "Okay, hear me out. There's this dancing monkey, you see.
      It wanders around the south end of town, spending its days dancing for
      people, but it seems so sad. It makes me wanna cry.

      I think someone should dance for the monkey for once. My boss would
      break my arm if I left work, but...could you do it for me?")

    (maybe-show-tutorial actor 'emote "You can perform simple gestures using the
      `emote` command. For example, you could type `emote dances for the
      monkey.` Type `help emote` for more information."))

  (:when-talk ((actor &quest dance-monkey :active) self topic)
    (tell self actor "Did you find the monkey? When you're done I won't make you
      do it all again, I promise."))

  (:when-talk ((actor &quest dance-monkey :done) self topic)
    (tell self actor "Look me in the eyes...you did it, didn't you? Ha ha I
      never thought you would, but thank you anyway!")
    (advance-quest self actor 'dance-monkey))

  (:when-talk (actor self topic)
    (tell self actor "Just look at these cobbles. Do you see the way they
      shine?")))

(deflocation bayside-plaza-3 (plaza-location)
  (:contents (young-laborer)
   :exits ((plaza-portal :west bayside-plaza-2 :east bayside-plaza-4)
           (dirt-road :south harbor-road-1))))

(deflocation bayside-plaza-4 (plaza-location)
  (:exits ((plaza-portal :west bayside-plaza-3 :east bayside-plaza-5)
           (rutted-path :north east-dock))))

(defquest ticket-please
  (:name "Ticket, Please"
   :summary "Grab a ticket from the dispenser and present it to the attendant.")
  (:active
      :summary "Grab a ticket from the dispenser.")
  (:done
    :summary "Present the ticket to the attendant."))

(defentity yellow-ticket (item)
  (:brief "a yellow ticket"
   :description "Tiny letters imprinted on the ticket read, \"May be exchanged
     for 30 silver coins. The issuer is not liable for any injury, death, or
     financial loss caused by goods or services purchased with said coins, or by
     the decision not to spend them.\""
   :quest ticket-please))

(defentity ticket-dispenser ()
  (:brief "a ticket dispenser"
   :icon flat-chest
   :description "A plaque atop the device reads:

    > This ticket dispenser is provided as a free service of the Explorer's Guild.")

  (:when-use ((actor &quest ticket-please :active) self target)
    ;; FIXME: receiveMessage "%tB rattles briefly then dispenses %ab."
    (receive actor self (list (clone-entity 'yellow-ticket)))
    (advance-quest self actor 'ticket-please)
    (maybe-show-tutorial actor 'give "You can use the `give` command to give an
      item to a non-player character. To complete this quest, give the ticket to
      the attendant."))

  (:when-use (actor self target)
    (show actor "Try as you might, you cannot activate the ticket dispenser.")))

(defentity attendant (humanoid)
  (:brief "an attendant"
   :description "The attendant is extremely tall and thin. He wears a long,
     striped coat and a comically tall tophat."
   :offers-quests (ticket-please))

  (:when-talk ((actor &quest ticket-please :available) self topic)
    (tell self actor "You look like one of those reborn heroes who keep popping
      up. I bet you need some coin. I'd be happy to be of service!")
    (offer-quest self 'ticket-please actor))

  (:after-accept-quest (actor (quest &quest ticket-please) self)
    (tell self actor "See that ticket dispenser over there? If you `use` it, it
      will give you a ticket. If you then give that ticket me, I will exchange
      it for 30 silver coins! It seems too good to be true, but I assure you the
      offer is real. Why not give it a try?")
    (maybe-show-tutorial actor 'use "The `use` command lets you use an item you
      are carrying or an item in your surroundings. It is a general-purpose way
      to employ an item for some effect. Try it on the ticket dispenser."))

  (:when-talk ((actor &quest ticket-please :active) self topic)
    (tell self actor "Go ahead and `use` the dispenser. It won't bite."))

  (:allow-give (actor self items)
    (every (lambda (e) (entity-isa e 'yellow-ticket)) items))

  (:after-give ((actor &quest ticket-please :done) self items)
    (tell self actor "Another ticket redeemed! My bonus this month is going to
      be off the charts! Here's your silver. Don't spend it all in one place!")
    (receive actor self (list (clone-entity 'silver-coin :quantity 30)))
    (advance-quest self actor 'ticket-please)))

(deflocation bayside-plaza-5 (plaza-location)
  (:contents (attendant ticket-dispenser)
   :exits ((plaza-portal :west bayside-plaza-4))))

;;; harbor road

(defentity harbor-road (location)
  (:name "Harbor Road"
   :description "A rutted dirt road runs between the harbor and the village
     proper."
   :surface :dirt
   :domain :outdoor))

(deflocation harbor-road-1 (harbor-road)
  (:exits ((dirt-road :north bayside-plaza-3 :south harbor-road-2)
           (entry-doorway :east miners-guildhall))))

(deflocation harbor-road-2 (harbor-road)
  (:exits ((dirt-road :north harbor-road-1 :south square-n))))

;;; miner's guild

(defentity marigold (mining-trainer)
  (:name "Marigold"
   :pose "is nearby, poking through a stack of ore."
   :description "Marigold is a cheerful, rosy-cheeked dwarven woman who looks
     strong enough to crush rocks. Her leather garments are streaked with soot.")

  (:when-talk (actor self topic)
    (tell self actor "Welcome to Arwyck! I am the local representative of the
      Underhill Consortium, a guild for those interested in mining. This
      workshop serves as a gathering place for our members.

      As a miner you can extract ore, raw gemstones, and other valuable
      resources from deposits scattered around the world. If this sounds useful,
      I'd love to welcome you to our guild! Type `guild info` for more
      information or `guild join` to sign up.

      If you decide mining's for you, Hermetch over there sells the tools
      you'll need to begin your mining career.")))

(defentity hermetch (vendor)
  (:name "Hermetch the Bald"
   :pose "stands in front of the tool rack, whistling idly."
   :description "Hermetch lives up to his apellation; his head shines almost as
     brightly as the polished blades of the pickaxes on the rack behind him."
   :sells (copper-pickaxe bronze-pickaxe))

  (:when-talk (actor self topic)
    (tell self actor "I sell the finest tools in the land! The absolute finest!")
    (with-delay (1)
      (show actor "Marigold rolls her eyes.")
      (with-delay (1)
        (tell self actor "Pay her no mind. Type `buy` to see what's
          available.")))))

(deflocation miners-guildhall ()
  (:name "Miners' Guildhall"
   :description "This cavernous warehouse contains numerous carts filled with
     different types of ore. A rack on the south wall holds a variety of
     pickaxes, shovels, and other tools useful for mining."
   :domain :indoor
   :surface :wood
   :contents (marigold hermetch)
   :exits ((exit-doorway :west harbor-road-1))))

;;; village square

(defentity square (location)
  (:name "Village Square"
   :description "This cobbled plaza is the heart of the village. Various shops
     line its perimeter."
   :domain :outdoor
   :surface :stone))

(defentity square-portal (continuing-portal)
  (:brief "the square"))

(defentity shaggy-dog (creature)
  (:brief "a shaggy white dog"
   :description "The dog is very curious and closely watches anyone who passes
     by. Its fur is matted and dirty but it seems happy.")

  (:after-enter-world ()
    (setf (? self :lying-down) (parse-verb "lies nearby, curiously eyeing passers-by.")
          (? self :standing) (parse-verb "stands nearby and looks around.")
          (? self :pose) (? self :standing)))

  (:after-enter-location (self location entry)
    (with-delay (10)
      (observe-event self :lay-down)))

  (:lay-down ()
    (show-observers (? (location self) :contents)
                    "The shaggy dog lays down in a comfortable spot.")
    (setf (? self :pose) (? self :lying-down))
    (with-delay ((random-integer 60 120))
      (observe-event self :stand-up)))

  (:stand-up ()
    (show-observers (? (location self) :contents)
                    "The shaggy dog stands up and stretches.")
    (setf (? self :pose) (? self :standing))
    (with-delay (10)
      (observe-event self :move-on)))

  (:move-on ()
    (if-let ((exits (remove-if-not (lambda (e)
                                     (when-let ((dest (symbol-value-or-nil (exit-dest e))))
                                       (entity-isa dest 'square)))
                                   (? (location self) :exits))))
      (traverse-portal self (location self) (random-elt exits))
      (observe-event self :lay-down)))

  (:when-talk (actor self topic)
    (show actor "The dog's ears perk up and it tilts its head to the side.")))

(deflocation square-nw (square)
  (:contents (shaggy-dog)
   :exits ((square-portal :east square-n :south square-w)
           (entry-doorway :north armory-training-hall :west sword-shop))))

(deflocation square-n (square)
  (:exits ((square-portal :west square-nw :east square-ne :south square-c)
           (dirt-road :north harbor-road-2))))

(deflocation square-ne (square)
  (:exits ((square-portal :west square-n :south square-e)
           (entry-doorway :east inn-common-room :north axe-shop))))

(deflocation square-w (square)
  (:exits ((square-portal :north square-nw :east square-c :south square-sw)
           (entry-doorway :west tower-library))))

(defentity square-sign ()
  (:brief "a directional sign"
   :pose "stands nearby."
   :description "Wooden arrows affixed atop the signpost read as follows:

     | North: Docks
     | West: Silverwood
     | South: Wall Street
     | East: Mistmarsh"))

(defentity square-spiritstone () ; FIXME: spiritstone
  (:pose "stands on a small pedestal in the center of the square."))

(deflocation square-c (square)
  (:contents (square-spiritstone square-sign)
   :exits ((square-portal :west square-w :east square-e
                          :north square-n :south square-s))
   :tutorial "You can `use` a spiritstone to bind your life essence to its
     location. If you later die, your spirit will return here and your body will
     be resurrected by the stone."))

(deflocation square-e (square)
  (:exits ((square-portal :north square-ne :west square-c :south square-se)
           (dirt-road :east east-road-1))))

(deflocation square-sw (square)
  (:exits ((square-portal :east square-s :north square-w)
           (entry-doorway :south lodge-foyer)
           (dirt-road :west forest-road-1))))

(deflocation square-s (square)
  (:exits ((square-portal :west square-sw :east square-se :north square-c)
           (dirt-road :south south-road-1))))

(defentity crone (humanoid)
  (:brief "an old crone"
   :pose "stands off to the side, watching people pass by."
   :description "The crone wears a faded gray cloak. Her wavy white hair spills
     from beneath her hood. Her eyes are bright, and she watches any passersby
     with great interest.")

  (:after-enter-location ((actor avatar) location entry)
    (show actor "The crone stares at you for a moment, then looks away."))

  (:when-talk (actor self topic)
    (tell self actor "Greetings, stranger. Your new body may be young, but I can
      sense the age of your spirit. This is not your first time around the
      block, so to speak. In a past life you were a great hero, but perhaps you
      have forgotten your deeds. A pity.

      Know this: you are here for a reason. This world needs you; I feel it in
      my bones, I hear it on the wind...but I know not why. What threat could be
      so grave that, in order to overcome it, we must tear history's heroes away
      from their peaceful slumber in the Dreamlands?

      Of course that raises another question: who or what is doing the tearing?

      In the village of my birth there is an old curse: \"May you live in
      interesting times.\" Perhaps I will be lucky and my days will end before
      things become too interesting. For you, though, I foresee no such
      luck.")))

(deflocation square-se (square)
  (:contents (crone)
   :exits ((square-portal :west square-s :north square-e))))

;;; sword shop

(defentity sword-vendor (vendor)
  (:name "Bryndan O'Donnel"
   :pose "cleans a longsword with an oiled rag."
   :description "Bryndan is a lanky man with freckled skin and gray eyes. His
     auburn hair hangs down to his shoulders."
   :sells (copper-shortsword bronze-shortsword)) ; FIXME: add more

  (:when-talk ((actor &quest talking-shop active) self topic)
    (tell self actor "Ah, yes, I heard about Mirabel. Those bastards stole a few
      blades from me, perhaps a week past.

      If you've not heard of the Gray Hand, well, that's a good thing. But every
      merchant in town knows 'em. They want a piece of the pie, and say they'll
      protect us. Little good they did this time. My contact there swears they
      had nothing to do with any of these recent thefts. But what good is the
      word of a thief?

      That being said, I've never had particular problems with the Hand before.
      So maybe this is something different.")
    ;; FIXME: (advance-quest self actor 'talking-shop 'sword-vendor)
    )

  (:when-talk (actor self topic)
    (tell self actor "Welcome to my shop! I have a variety of swords for sale.
      Type `buy` to see what I have available.")))

(deflocation sword-shop ()
  (:name "Bryndan's House of Blades"
   :description "A variety of bladed weapons are displayed in racks that fill
     this small shop."
   :domain :indoor
   :surface :wood
   :contents (sword-vendor)
   :exits ((exit-doorway :east square-nw))))

;;; axe shop

(defentity axe-vendor (vendor)
  (:name "Durg Bighands"
   :pose "sits atop a chair that is clearly too small for him."
   :description "Durg is an ogre. Although he is very small for his kind, he is
     still far too large for the typical furniture found in Arwyck."
   :sells (copper-axe bronze-axe copper-battle-axe bronze-battle-axe))

  (:when-talk (actor self topic)
    (show actor "Durg stretches his shoulders.")
    (tell self actor "Do you happen to need an axe? Maybe two? Type `buy` to see
      my wares.")))

(deflocation axe-shop ()
  (:name "Durg's Slicery"
   :description "The walls are lined with inexpensive but servicable axes."
   :domain :indoor
   :surface :wood
   :contents (axe-vendor)
   :exits ((exit-doorway :south square-ne))))

;;; inn

(defentity inn-room (location)
  (:domain  :indoor
   :surface :wood
   :subregion "Golden Gryphon"))

(defentity rye-loaf (item) ; FIXME: (food)
  (:brief "a loa[f|ves] of rye bread"
   :description "The loaf of rye bread looks rather delicious."
   :icon bread
   :stackable t
   :price (2 silver-coin)))

(defentity fish-soup (item) ; FIXME: (food)
  (:brief "a bowl[s] of fish soup"
   :description "Warning: Fish content not guaranteed."
   :stackable t
   :price (3 silver-coin)))

(defentity sienna (vendor)
  (:name "Sienna"
   :pose "is working behind the bar."
   :description "Sienna is a tall woman with short yellow hair and light brown
     eyes. She keeps a close watch over her patrons."
   :sells (rye-loaf fish-soup))

  (:when-talk (actor self topic)
    (tell self actor "Welcome to the Golden Gryphon! I sell food and drink. I'd
      say it's the best in town but my mam told me never to lie, so I won't. If
      you're learning to cook, I can sell you a few recipes as well. Type `buy`
      to see what I have for sale.")))

(deflocation inn-common-room (inn-room)
  (:name "Common Room"
   :description "The common room of the Golden Gryphon is a cheerful place. A
     crackling fire burns in the stone hearth. Numerous tables and chairs give
     the space a crowded feel, even when few customers are present. A long oak
     bar lines one wall."
   :contents (sienna)
   :exits ((exit-doorway :west square-ne)
           (doorway :north inn-kitchen)
           (stairway :up inn-upstairs-hall))))

(defentity dully (humanoid) ; FIXME: (cooking-trainer)
  (:brief "Dully the Cook"
   :pose "is stirring a pot of...something."
   :description "Dully is a portly dwarven male with a scraggly beard. He wears a
     grease-stained apron and a strange, poofy hat. He wields a huge wooden spoon
    as if it were a weapon."
   :teaches nil) ; FIXME: from cooking-trainer

  (:after-enter-location (actor location entry)
    (tell self actor "Aye, it's a good day for a stew, ain't it?"))

  (:when-talk (actor self topic)
    (tell self actor "Aye, if ye be lookin' ta learn ta cook, I be yer dwarf.
      Type `learn` ta see wha I can teach ya, or `help skills` ta learn more
      about skills an' such.")))

(deflocation inn-kitchen (inn-room)
  (:name "Kitchen"
   :description "This large and somewhat disorganized kitchen smells faintly of
     ale and grease."
   :contents (dully)
   :exits ((doorway :south inn-common-room))))

(deflocation inn-upstairs-hall (inn-room)
  (:name "Upstairs Hall"
   :description "This dark, windowless hall leads to the Golden Gryphon's guest
     rooms."
   :exits ((stairway :down inn-common-room)
           (doorway :west small-guest-room :east large-guest-room))))

(deflocation small-guest-room (inn-room)
  (:name "Small Guest Room"
   :description "This cramped room holds a small bed and an iron-bound chest.
     The window overlooks the village square."
   :exits ((doorway :east inn-upstairs-hall))))

(deflocation large-guest-room (inn-room)
  (:name "Large Guest Room"
   :description "This room is tastefully furnished. The wide feather bed is
     lumpy but comfortable. An oak wardrobe features carvings of trees and
     forest creatures. The window provides a view of nearby rooftops and the
     rolling hills east of the village."
   :exits ((doorway :west inn-upstairs-hall))))

;;; armory

(defentity andalya (humanoid) ; FIXME: (warrior-trainer)
  (:name "Andalya"
   :pose "is in the center of the room, idly swinging a slender longsword."
   :description "Andalya is a lean, graying woman who wears loose clothing
     befitting one who teaches the martial arts.")

  (:when-talk (actor self topic)
    (tell self actor "Welcome, stranger. I am Andalya. It is my privilege to be
      the local representative of the Company of the Blade. We are a band
      dedicated to the practice of the martial arts. It is my job to train those
      who are interested in learning to defend themselves with good steel.

      Type `guild info` to learn more about my guild. If you want to join, type
      `guild join`.")))

(deflocation armory-training-hall ()
  (:name "Training Hall"
   :description "This long hall has stone walls and an arched ceiling. The soft
     wooden floor is clean but marred from the many training matches that have
     been fought here."
   :domain :indoor
   :surface :wood
   :contents (andalya)
   :exits ((exit-doorway :south square-nw))))

;;; explorers' lodge

(defentity lodge-room (location)
  (:domain :indoor
   :surface :wood
   :subregion "Explorers' Lodge"))

(defentity quartermaster (vendor)
  (:brief "the quartermaster"
   :pose "stands nearby."
   :description "The quartermaster is a slender man with graying hair and
     elegant mustaches. His clothing, although impeccably clean, seems a little
     too big for him."
   :sells nil) ; FIXME:

  (:when-talk (actor self topic)
    (tell self actor "Good day to you, traveler.")))

(deflocation lodge-foyer (lodge-room)
  (:name "Foyer"
   :description "This airy room has wood-paneled walls and a thick carpet on the
     floor."
   :contents (quartermaster)
   :exits ((exit-doorway :north square-sw)
           (doorway :south lodge-trophy-room :west lodge-study))))

(defentity stuffed-gorilla ()
  (:brief "a stuffed gorilla"
   :pose "stands at the end of the room."
   :icon gorilla
   :description "This intimidating creature stands over eight feet tall. Even in
     death its expression is fierce. A plaque mounted on the wall reads:

     > Although it slew several dogs and gravely wounded a scout, our party
     defeated this beast as we traversed the jungles of Phaa, CY 549."))

(deflocation lodge-trophy-room (lodge-room)
  (:name "Trophy Room"
   :description "The walls of this long room are lined with hunting prizes of
     all kinds: antlers, horns, mounted heads, and more."
   :contents (stuffed-gorilla)
   :exits ((doorway :north lodge-foyer :west lodge-workshop))))

(defentity lord-olmquist (humanoid)
  (:brief "Lord Olmquist"
   :pose "sits in a chair, smoking a pipe."
   :description "Olmquist is a rather stout, red-cheeked fellow with a bushy
     white beard and bald pate. Horn-rimmed spectacles perch upon his
     substantial nose. He wears a velvet smoking jacket, an open-necked white
     shirt, and dark silk trousers.")

  (:when-talk (actor self topic)
    (tell self actor "Why yes, be a dear and fetch me a bourbon, won't you?")
    (with-delay (1)
      (show actor "You look around, but there's no bourbon in sight."))))

(deflocation lodge-study (lodge-room)
  (:name "Study"
   :description "Two walls of this room are lined with shelves full of books. A
     number of comfortable armchairs and small tables fill the remainder of the
     space."
   :contents (lord-olmquist)
   :exits ((doorway :east lodge-foyer :south lodge-workshop))))

(defentity naman (humanoid) ; FIXME: (explorer-trainer)
  (:name "Naman Artani"
   :pose "is hunched over a strange device on the table."
   :description "Naman is a wiry young man who wears a scarred leather apron
     over his practical attire. His curly auburn hair floats in a wild cloud
     around his head."
   :teaches nil) ; FIXME: from explorer-trainer. swimming, climbing, ...?

  (:when-talk (actor self topic)
    (tell self actor "Welcome to the Explorer's Lodge. Members of our guild
      travel across Atalea in search of rarities: strange creatures, lost
      treasures, and ancient knowledge. As you might imagine, survival skills
      come in quite handy in this vocation!

      I am happy to teach you some of those skills. Type `learn` to see what I
      can teach you.")))

(deflocation lodge-workshop (lodge-room)
  (:name "Workshop"
   :description "A long table fills the center of this room; various implements
     are strewn across its surface. Shelves along the walls hold all manner of
     tools and contraptions."
   :contents (naman)
   :exits ((doorway :north lodge-study :east lodge-trophy-room :south wall-street-1))))

;;; mages' tower

(defentity tower-room (location)
  (:domain :indoor
   :surface :stone
   :subregion "Mages' Tower"))

(deflocation tower-library (tower-room)
  (:name "Library"
   :description "This large, circular room encompasses the entire ground floor
     of a squat, stone tower. The walls are lined with shelves full of
     leather-bound books."
   :exits ((exit-doorway :east square-w)
           (stairway :down tower-basement :up tower-study))))

(defentity giglox (vendor)
  (:name "Giglox Tiglox"
   :pose "excitedly awaits your attention."
   :description "Giglox is a tiny, long-eared creature. His toothy smile is both
     infectious and somewhat frightening."
   :sells (pine-wand oak-wand))

  (:when-talk (actor self topic)
    (tell self actor "You look like you need a wand. And who doesn't? Type `buy`
       to see what I have available.")))

(deflocation tower-basement (tower-room)
  (:name "Basement"
   :description "The basement of the library is full of books, scrolls, and
     other scholarly items. Everything is neatly stacked and organized, if a
     little dusty."
   :contents (giglox)
   :exits ((stairway :up tower-library))))

(defentity milena (humanoid) ; FIXME: (mage-trainer)
  (:name "Milena Landeris"
   :pose "sits at a nearby desk, perusing a scroll."
   :description "Milena is pleasant-looking young woman. Her clothing is casual
     and practical. Her long black hair is carefully plaited. She seems somewhat
     bored with the scroll in her hands and her eyes often wander to the nearest
     window."
   :teaches nil) ; FIXME: from mage-trainer

  (:when-talk (actor self topic)
    (tell self actor "Hello! I'm glad for the interruption. I am supposed to be
      studying this scroll, but the mating rituals of the southern blue-tailed
      cockatrice are really not my cup of tea.

      I am the local representative of the College of Arcanists, a guild for
      those who wish to study magic. I am just an apprentice myself, but I can
      teach you some basic skills; type `learn` to see what I can teach. If you
      are new in town, you might want to type `help skills` to learn more about
      skills in general.")))

(deflocation tower-study (tower-room)
  (:name "Study"
   :description "This circular room commands an excellent view of Arwyck through
     a number of large, glass-paned windows. A desk and padded leather chair
     have been placed beneath each window."
   :contents (milena)
   :exits ((stairway :down tower-library))))

;;; east road

(defentity east-road (location)
  (:name "East Road"
   :description "This broad dirt road connects the center of Arwyck to the
     village's eastern gate."
   :domain :outdoor
   :surface :dirt))

(deflocation east-road-1 (east-road)
  (:exits ((dirt-road :west square-e :east east-road-2)
           (entry-doorway :south mace-shop))))

(defquest i-need-bread
  (:name "Feed the Poor"
   :level 1
   :summary "Purchase a loaf of rye bread and give it to the hungry street
     urchin.")

  (:active
      :summary "Give a loaf of rye bread to the street urchin."))

(defentity urchin (humanoid)
  (:brief "a street urchin"
   :pose "sits alongside the road."
   :description "The urchin is a young girl, perhaps eight years old. Her
     clothes are little more than dirty rags."
   :offers-quests (i-need-bread))

  (:when-talk ((actor &quest i-need-bread :available) self topic)
    (tell self actor "You seem like you have many important things to do, so I
      hate to bother you. But could you spare a loaf of bread? My family is
      desperate for food, you see, and I wasn't able to beg for enough coin to
      buy any.")
    (offer-quest self 'i-need-bread actor))

  (:after-accept-quest (actor (quest &quest i-need-bread) self)
    (tell self actor "Oh, thank you! If you go to the inn, the innkeeper sells a
      very nice loaf of rye. I'd really appreciate if you could `give` me
      one."))

  (:when-talk ((actor &quest i-need-bread :active) self topic)
    (show actor "The urchin looks at you with a hint of expectation.")
    (tell self actor "Were you able to find the inn?"))

  (:allow-give (actor self items)
    (every (lambda (e) (entity-isa e 'rye-loaf)) items))

  (:after-give ((actor &quest i-need-bread :active) self items)
    (show actor "The urchin grins from ear to ear.")
    (tell self actor "Thank you so much! You've truly done my family a great
      service today. May the gods' blessings return to us all!")
    (advance-quest self actor 'i-need-bread))

  (:after-give (actor self items)
    (tell self actor "Bread is always appreciated! Thanks so much for your
      kindness!")))

(deflocation east-road-2 (east-road)
  (:contents (urchin)
   :exits ((dirt-road :west east-road-1 :east east-road-3)
           (alley :south muggers-alley-1))))

(deflocation east-road-3 (east-road)
  (:exits ((dirt-road :west east-road-2 :east east-gate)
           (entry-doorway :north temple-sanctuary))))

;;; mace shop

(defentity maury (vendor)
  (:name "Maury na Munigan"
   :pose "stands behind the counter, arms crossed."
   :description "Maury is a burly man with a full beard and little hair left
     atop his head. He looks strong enough to crack a few skulls without the aid
     of the weapons he sells."
   :sells (copper-mace bronze-mace copper-maul bronze-maul))

  (:when-talk (actor self topic)
    (tell self actor "Welcome to my shop, friend. You'll find my wares to be of
      better quality than most, I think.")))

(deflocation mace-shop ()
  (:name "Maury's Maces"
   :description "This shop sells a variety of weapons ideal for cracking
     skulls."
   :domain :indoor
   :surface :wood
   :contents (maury)
   :exits ((exit-doorway :north east-road-1))))

;;; temple

(defentity old-priest (humanoid)
  (:brief "an aged priest"
   :pose "kneels before the altar."
   :description "The priest is unkempt and dirty. His robes are in desperate
     need of repair and his scraggly beard is in dire need of a trim. You can
     see intelligence and compassion in his striking blue eyes, but also a touch
     of madness.")

  (:when-talk (actor self topic)
    (tell self actor "Be welcome in this temple, friend.

      My flock is few in number these days. As the gods abandoned the people, so
      too did the people abandon the gods. I do not blame them. As for myself, I
      cannot turn my back on a lifetime of belief. Not yet.")

    (with-delay (1)
      (show actor "The priest pauses for a moment, collecting his thoughts.")

      (with-delay (3)
        (tell self actor "It began slowly at first, nearly half a century ago.
          The gods had always lent their power to their faithful servants.
          Members of the priesthood wielded magic as powerful as that of any
          mage.

          But then our powers began to fail. One by one, the gods stopped
          granting us their blessings. Did they lose interest in us? Did they go
          elsewhere? Perhaps they simply ceased to exist?

          It is heresy to think such things. But now it has been twenty years
          since the last of them disappeared. My order no longer wields any
          power and we can do little to help the people we once protected.
          Perhaps we no longer deserve the gods' aid and guidance? I do not
          know, but I will continue my search for answers.

          The absence of the gods leaves a great void. My only hope is that evil
          and darkness do not fill it.")))))

(deflocation temple-sanctuary ()
  (:name "Sanctuary"
   :description "This large room was once a place where people gathered to
     worship the gods, but it has clearly not been used in many years. A large
     altar stands near one end of the room and various statues reside in niches
     along the walls. Dust and grime cover every surface; puddles on the floor
     mark the spots where the wooden roof has begun to fail. The tapestries that
     once lined the walls have fallen into ruin."
   :domain :indoor
   :surface :stone
   :contents (old-priest)
   :exits ((exit-doorway :south east-road-3))))

;;; east gate

(defentity east-guard (guard)
  (:pose "leans against the wall of the gatehouse.")

  (:when-talk ((actor &quest at-the-gates :actor) self topic)
    (tell self actor "Mirabel's boy? Yes, he passed through in the dead of
      night, maybe three weeks past. As shady as ever, that one. He was with two
      others who looked even worse.")
    (advance-quest self actor 'at-the-gates 'guard)) ; FIXME:

  (:when-talk (actor self topic)
    (tell self actor "East of here is Mistmarsh. It's a dangerous area. Don't
      fall in a sinkhole...I'm not comin' to drag you out!")))

(deflocation east-gate ()
  (:name "East Gate"
   :description "A squat stone gatehouse stands at the village's entrance. To
     the east, a road winds through marshes that stretch along the coast of
     Emerald Bay."
   :domain :outdoor
   :surface :stone
   :contents (east-guard)
   :exits ((dirt-road :west east-road-3 :east jade.mistmarsh::muddy-path-a12))))

;;; muggers' alley

(defentity muggers-alley (location)
  (:name "Muggers' Alley"
   :description "This narrow, muddy lane cuts through the least savory section
     of Arwyck."
   :domain :outdoor
   :surface :dirt))

(deflocation muggers-alley-1 (muggers-alley)
  (:exits ((alley :north east-road-2 :south muggers-alley-2)
           (entry-doorway :east potion-shop))))

(deflocation muggers-alley-2 (muggers-alley)
  (:exits ((alley :north muggers-alley-1 :south muggers-alley-3)
           (entry-doorway :west dagger-shop))))

(deflocation muggers-alley-3 (muggers-alley)
  (:exits ((alley :north muggers-alley-2 :south wall-street-6)
           (entry-doorway :east warehouse-anteroom :west crow-bar))))

;;; potion shop

(defentity gemma (vendor)
  (:name "Gemma"
   :pose "cleans a clay jar with a greasy cloth."
   :description "Gemma --- the alchemist's daughter --- is a pudgy young woman
     who looks extremely bored with her work."
   :sells nil) ; FIXME:

  (:when-talk (actor self topic)
    (tell self actor "Hello there. What brings you in? Supplies have been rather
      hard to come by lately, but I have a few potions to sell. Type `buy` to
      see what's in stock.")))

(deflocation potion-shop ()
  (:name "Dilwar's Elixiry"
   :description "The north wall is lined with shelves, but they hold only a few
     lonely jars and bottles."
   :domain :indoor
   :surface :wood
   :contents (gemma)
   :exits ((exit-doorway :west muggers-alley-1)
           (doorway :east alchemy-workshop))))

(defquest get-some-venom
  (:name "It's Centipedal"
   :summary "Kill giant centipedes in the Mistmarsh and return their venom to
     Dilwar."
   :level 2)

  (:active
      :summary "Use the venom extractor to obtain three units of venom from the
        corpses of giant centipedes.")

  (:done
      :summary "Talk to Dilwar."))

(defentity venom-extractor (item)
  (:brief "a venom extractor"
   :description "This contraption can be used to extract a vial of venom from fresh
     corpses of certain creatures."
   :quest get-some-venom)

  (:allow-use (actor self (target jade.mistmarsh::giant-centipede-corpse))
    t)

  (:when-use (actor self (target jade.mistmarsh::giant-centipede-corpse))
    (show actor "You stick the needle of the venom extractor into the corpse and
      pull the trigger. The device sucks a small amount of venom into an
      internal chamber.")
    (advance-quest self actor 'get-some-venom 1/3)))

(defentity dilwar (humanoid)
  (:brief "Dilwar"
   :pose "stares at the smoking remains of a shattered bowl, lost in thought."
   :description "The alchemist is a rotund, red-faced man with wisps of white
     hair that float over his head like smoke."
   :offers-quests (jade.silverwood::moss-results get-some-venom))

  (:when-talk ((actor &quest get-some-venom :available) self topic)
    (tell self actor "Can't you see I'm busy here?")
    (with-delay (1)
      (show actor "The alchemist scratches his chin.")
      (tell self actor "On second thought, you might be of use...unless you're
        scared of bugs. Big bugs. Very, very big bugs.")
      (offer-quest self 'get-some-venom actor)))

  (:after-accept-quest (actor (quest &quest get-some-venom) self)
    (tell self actor "As you might have noticed, our inventory is rather
      depleted. I was mixing up some new love potions, but I seem to have blown
      them up. Faulty equipment, I'm sure.

      Sadly, that was the last of my centipede venom. I'll pay you well if you
      can bring me some. Here, take this ... you'll need it.")

    (receive actor self (list (clone-entity 'venom-extractor)))

    (tell self actor "Use the device on three centipede corpses and you'll get
      plenty of venom. You can find centipedes in the Mistmarch, east of
      town."))

  (:when-talk ((actor &quest get-some-venom :active) self topic)
    (tell self actor "Please stop wasting my time. I need that venom."))

  (:when-talk ((actor &quest get-some-venom :done) self topic)
    (tell self actor "Yes, this will do quite nicely. A job well done. You've
      earned your reward.")
    (receive actor self (list (clone-entity 'silver-coin :quantity 20)))
    (advance-quest self actor 'get-some-venom)))

(deflocation alchemy-workshop ()
  (:name "Dilwar's Workshop"
   :description "This small room is packed with glassware, tubes, and esoteric
     materials stored in haphazardly-arranged bins and jars."
   :domain :indoor
   :surface :wood
   :contents (dilwar)
   :exits ((doorway :west potion-shop))))

;;; dagger shop

(defentity jimmy (vendor)
  (:name "Jimmy the Hare"
   :pose "cleans his fingernails with a tiny silver knife."
   :description "Jimmy is a greasy-haired young man whose eyes dart around
     nervously, as if he expects someone to leap from the shadows at any
     moment."
   :sells (copper-dagger bronze-dagger))

  (:when-talk ((actor &quest talking-shop :active) self topic)
    (tell self actor "Mirabel? It's a bleedin' shame, it is. Now, the Gray Hand
      is bad enough, but they've got honor after a fashion. Pay their price, you
      see, and they leave you alone. In a small town like this, the price is not
      unbearable for a prosperous business. Or so I hear.

      This job ain't the Hand's style. Someone else must be workin' town.
      I pity them if the Hand gets its, uh, hands on them...")
    (advance-quest self actor 'talking-shop 'dagger-vendor))

  (:when-talk (actor self topic)
    (tell self actor "Hey stranger, keep yer distance. If ya be needin' a
      dagger, yer in the right place. Pick one ya like an' leave yer silver on
      the table on yer way out.")))

(deflocation dagger-shop ()
  (:name "Shivs n' Such"
   :description "This shop sells all manner of knives and daggers."
   :domain :indoor
   :surface :wood
   :contents (jimmy)
   :exits ((exit-doorway :east muggers-alley-2))))

;;; crow bar

(defentity barkeep (vendor)
  (:brief "a barkeep"
   :pose "stands behind the counter."
   :sells nil)) ; FIXME:

(defentity scarecrow (humanoid)
  (:brief "the scarecrow"
   :pose "sits alone at a table."
   :description "The scarecrow is a gaunt young man with a nest of straw-colored
     hair. His clothes are dirty and ill-fitting.")

  (:allow-use ((actor &quest scare-the-scarecrow :scare)
               (item jade.mistmarsh::giant-centipede-head) self)
    t)

  (:when-use ((actor &quest scare-the-scarecrow :scare)
              (item jade.mistmarsh::giant-centipede-head) self)
    (show actor "The scarecrow recoils in horror.")
    (tell self actor "Gah! Get that thing away from me and I'll tell you
      anything!")
    (with-delay (1)
      (show actor "You ask the scarecrow about Evend.")
      (with-delay (1)
        (tell self actor "Evend? Yeah, he's back. He brought two goons with him.
          They're holed up in a shack along the road that leads toward the
          forest, west of here. He's changed...he was always a little dark, but
          now he honestly scares me a little.")
        (advance-quest self actor 'scare-the-scarecrow)))))

(deflocation crow-bar ()
  (:name "The Crow Bar"
   :description "This shadowy, smoke-filled room contains a handful of
     mismatched tables and chairs. A stained wooden counter runs along the west
     wall."
   :domain :indoor
   :surface :wood
   :contents (barkeep scarecrow)
   :exits ((exit-doorway :east muggers-alley-3)))

  (:after-enter-location ((actor &quest scare-the-scarecrow :scare) location entry)
    (maybe-show-tutorial
     actor 'scare
     "To scare the scarecrow, just `use` a giant centipede head on him.")))

;;; warehouse

(defentity warehouse (location)
  (:domain :indoor
   :surface :wood
   :subregion "Warehouse"))

(deflocation warehouse-anteroom (warehouse)
  (:name "Anteroom"
   :description "This cramped room has stained walls and an uneven wooden floor.
     The air is heavy with smoke."
   :exits ((exit-doorway :west muggers-alley-3)
           (doorway :east warehouse-storeroom)
           (stairway :up warehouse-office))))

(defentity shady-roger (humanoid) ; FIXME: (vagabond-trainer)
  (:brief "Shady Roger"
   :pose "sits in a chair with his feet on the desk."
   :description "Roger is a scrawny man with three days' stubble. His gaze is
     constantly scanning the room, as if he expects an enemy to appear at any
     moment. He wears well-worn leather garments and a bandolier of knives
     across his chest.")

  (:when-talk (actor self topic)
    (tell self actor "I see you found our secret lair. Well, not so secret,
      really; if it were we'd have no recruits!

      I am a representative of the Gray Hand. My brothers and sisters work to
      achieve our goals through subtlety, misdirection, and the occasional knife
      in the back. Brute force is for suckers! As for magic, well ... not
      everyone likes to spend their days reading musty old scrolls. I'd rather
      be out in the streets, where the action is!

      I can teach you some of the basic skills favored by my guild. Type `guild
      info` for more information, or type `learn` to see what you can learn from
      me.")))

(deflocation warehouse-office (warehouse)
  (:name "Office"
   :description "A huge mahogany desk dominates this small room. Shelves along
     the walls are packed with numerous scrolls, ledgers, and other documents."
   :contents (shady-roger)
   :exits ((stairway :down warehouse-anteroom))))

(deflocation warehouse-storeroom (warehouse)
  (:name "Storeroom"
   :description "This large room has a high ceiling supported by heavy beams.
     All shapes and sizes of crates, barrels, and boxes are stacked on the
     floor."
   :exits ((doorway :west warehouse-anteroom :east warehouse-meeting-room))))

(deflocation warehouse-meeting-room (warehouse)
  (:name "Meeting Room"
   :description "This cramped room contains a long table and numerous chairs and
     stools."
   :exits ((doorway :west warehouse-storeroom)))

  ;; FIXME: There was a fire trap upon entering...?
  )

;;; south road

(defentity south-road (location)
  (:name "South Road"
   :description "This dirt road connects the village square with the south
     gate."
   :domain :outdoor
   :surface :dirt))

(deflocation south-road-1 (south-road)
  (:exits ((dirt-road :north square-s :south south-road-2)
           (entry-doorway :east looted-shop))))

(deflocation south-road-2 (south-road)
  (:exits ((dirt-road :north south-road-1 :south wall-street-3)
           (entry-doorway :east spear-shop))))

;;; looted shop

(defentity mirabel (humanoid)
  (:name "Mirabel"
   :pose "sweeps broken glass from the floor."
   :description "Mirabel is a middle-aged woman with chestnut hair in a tight
     bun."
   :offers-quests (talking-shop at-the-gates scare-the-scarecrow
                                the-key-is-the-key the-end-of-evend))

  (:when-talk ((actor &quest the-end-of-evend :finished) self topic)
    (tell self actor "Hello again, ~a. I hope you're staying out of trouble. I'm
      still picking up the pieces, but life goes on. Take care."
          (? actor :name))))

(deflocation looted-shop ()
  (:name "Looted Shop"
   :description "It's hard to tell what this shop sells, since its entire
     inventory appears to have been stolen. The thieves also damaged the
     furniture and broke those few items they did not take."
   :domain :indoor
   :surface :wood
   :contents (mirabel)
   :exits ((exit-doorway :west south-road-1))))

;; Evend quest chain

(defquest talking-shop
  (:name "Talking Shop"
   :level 2
   :summary "Talk to shopkeepers around Arwyck who may have information about the
     recent spate of thefts, then report your findings to Mirabel.")

  (:active
      :summary "Talk to shopkeepers around Arwyck who may have information about
        the recent spate of thefts."
      :initial-state '((sword-vendor . 0) (dagger-vendor . 0) (spear-vendor . 0)))

  (:done
      :summary "Report your findings to Mirabel."))

(defbehavior mirabel
  (:when-talk ((actor &quest talking-shop :available) self topic)
    (tell self actor "I'm sorry, but we're closed. As you can see,
        everything's been stolen. But you look like a capable sort; maybe you
        can help me out?")
    (offer-quest self 'talking-shop actor))

  (:after-accept-quest (actor (quest &quest talking-shop) self)
    (tell self actor "You're a gods-send. As you may be aware, thieves have
      broken into a number of local shops recently. In most cases they took a
      few valuables and caused a little damage. In my case, however, they were
      less kind ... I've no idea why.

      Please talk to some of the other shopkeepers. Maybe they have some
      information that can help me figure out why this happened to me?"))

  (:when-talk ((actor &quest talking-shop :active) self topic)
    (tell self actor "Have you talked to the other shopkeepers yet? They should
      be marked on your map, if that helps."))

  (:when-talk ((actor &quest talking-shop :done) self topic)
    (show actor "You share your findings with Mirabel.")
    (with-delay (2)
      (tell self actor "Thank you for your help. If my peers are correct, these
        thefts are the work of a new player in town. I can't help but think my
        wayward son may be involved.

        From the look on your face, I can see I owe you an explanation.

        I moved here from Irridel with my son, Evend, when he was a young boy.
        Irridel, as you may know, is a much larger town to the south. Evend
        hated it here and never forgave me for the move. Our relationship slowly
        soured until, one day, we had a huge argument. He left that night,
        swearing he wanted nothing more to do with Arwyck or with me.

        He was always a bitter, vengeful boy. In that way he takes after his
        late father. If Evend is back in town, I'd like to know about it. I have
        some friends who might know something more.")
      (advance-quest self actor 'talking-shop))))

(defquest at-the-gates
  (:name "At the Gates"
   :level 2
   :required-quests (talking-shop)
   :summary "Talk to the gate guards in Arwyck to see if Mirabel's son has been
     seen entering the town.")

  (:active
    :summary "Ask the gate guards in Arwyck if they've seen Mirabel's son."
    :initial-state) ; FIXME: guards

  (:done
    :summary "Report your findings to Mirabel."))

(defbehavior mirabel
  (:when-talk ((actor &quest at-the-gates :available) self topic)
    (tell self actor "You've already been a great help, but I think I know how
      we can determine if my son has come back to town.")
    (offer-quest self 'at-the-gates actor))

  (:after-accept-quest (actor (quest &quest at-the-gates) self)
    (tell self actor "There are only three ways into town, if we ignore the
      docks --- my son was always deathly afraid of the sea. It just so happens
      that I am friends with the guards at all three gates.

      Go talk to the guards. If my son has dragged himself back to Arwyck, one
      of them will have seen him."))

  (:when-talk ((actor &quest at-the-gates :active) self topic)
    (tell self actor "All three guards are very observant. You'll find them at
      the city gates to the west, south, and east."))

  (:when-talk ((actor &quest at-the-gates :done) self topic)
    (show actor "Mirabel considers for a moment.")
    (with-delay (2)
      (tell self actor "So, he's back, and he brought friends. This can't be
        good.

        No doubt you've heard of the Gray Hand. Evend always wanted to join
        them, but was rebuffed. It's surely not a good sign when even thieves
        doubt your character.

        Arwyck is a tiny place. A small gang with some ambition can cause a lot
        of trouble. I know he's my son, but we need to put an end to this before
        it gets out of hand.")
      (advance-quest self actor 'at-the-gates))))

(defquest scare-the-scarecrow
  (:name "Scare the Scarecrow"
   :summary "Find a giant centipede head in Mistmarsh and use it to scare the
     Scarecrow into providing information about Mirabel's son."
   :level 2
   :required-quests (at-the-gates))

  (:get-ahead
    :summary "Hunt giant centipedes in Mistmarsh to obtain one of their heads.")

  (:scare
    :summary "Find the scarecrow and use the giant centipede head to scare
      him.")

  (:done
    :summary "Return to Mirabel and tell her what you've learned."))

(defbehavior mirabel
  (:when-talk ((actor &quest scare-the-scarecrow :available) self topic)
    (tell self actor "We need to find where Evend is hiding. If you're willing
      to get your hands dirty, I think we can find him.")
    (offer-quest self 'scare-the-scarecrow actor))

  (:after-accept-quest (actor (quest &quest scare-the-scarecrow) self)
    (tell self actor "There's a character in town called the Scarecrow. He and
      Evend were friends. He's well connected among the less savory types in
      town.

      Normally I wouldn't expect him to rat out a friend, but the Scarecrow has
      a weakness ... centipedes. He had a bad experience as a child, I guess.

      Scare him with a giant centipede head and I'm sure he'll become talkative.
      Look for the centipedes in the swamp to the east."))

  (:when-talk ((actor &quest scare-the-scarecrow :get-ahead) self topic)
    (tell self actor "Any luck finding a giant centipede head?"))

  (:when-talk ((actor &quest scare-the-scarecrow :scare) self topic)
    (tell self actor "Any luck with the scarecrow? I wish I could see his face
      when you toss that giant head in his lap!"))

  (:when-talk ((actor &quest scare-the-scarecrow :done) self topic)
    (tell self actor "Well, now we know where he is. All that's left is to get
      in, and I know just the man to help us.")
    (advance-quest self actor 'scare-the-scarecrow)))

(defquest the-key-is-the-key
  (:name "The Key is the Key"
   :summary "Get the items Kijian requires, then obtain a key to Evend's hideout."
   :level 2
   :required-quests (scare-the-scarecrow))

  (:find-kijian
    :summary "Talk to Kijian the locksmith.")

  (:gather-items
    :summary "Give Kijian a silky spiderweb and a kobold trinket."
    :initial-state ((silky-spiderweb . 0) (kobold-trinket . 0)))

  (:key-ready
    :summary "Talk to Kijian to obtain the key.")

  (:done
    :summary "Take the key to Mirabel."))

(defbehavior mirabel
  (:when-talk ((actor &quest the-key-is-the-key :available) self topic)
    (tell self actor "I'm sure Evend keeps his hideout locked up tight, but it
      so happens I'm friends with the only locksmith in town.")
    (offer-quest self 'the-key-is-the-key actor))

  (:after-accept-quest (actor (quest &quest the-key-is-the-key) self)
    (tell self actor "Go pay a visit to Kijian, the locksmith. He made just
      about every lock in this town, and I'm sure he can help us. Be warned,
      he's likely going to require something in return. You'll find his shop
      just inside the wall on the south end of town."))

  (:when-talk ((actor &quest the-key-is-the-key :find-kijian) self topic)
    (tell self actor "Was Kijian willing to help?"))

  (:when-talk ((actor &quest the-key-is-the-key :done) self topic)
    (tell self actor "That's the key, then? The end is near. We've only one
      thing left to do, and it is by far the hardest.")
    (advance-quest self actor 'the-key-is-the-key)))

(defquest the-end-of-evend
  (:name "The End of Evend"
   :summary "Enter Evend's hideout and defeat his gang."
   :level 2
   :required-quests (the-key-is-the-key))

  (:active
    :summary "Defeat Evend and his gang."
    :initial-state nil) ; FIXME: ['evend, 'goon, 'ruffian]

  (:done
    :summary "Report to Mirabel."))

(defentity mirabels-ring (item) ; FIXME: ring
  (:name "Mirabel's ring"
   :description "This ring is one that Mirabel received from her late husband.
     It is a simple copper band etched with a delicate pattern of twisting
     vines."
   :level 2
   :traits (:vitality 3)))

(defbehavior mirabel
  (:when-talk ((actor &quest the-end-of-evend :available) self topic)
    (tell self actor "It breaks my heart to say this, but my son must be taught
      a lesson. And you look like a willing teacher.")
    (offer-quest self 'the-end-of-evend actor))

  (:after-accept-quest (actor (quest &quest 'the-end-of-evend) self)
    (tell self actor "I fear you know what must be done. Head west, find Evend's hideout,
      and defeat him. Watch out for his friends, they'll surely fight by his
      side. I wish you luck."))

  (:when-talk ((actor &quest the-end-of-evend :active) self topic)
    (tell self actor "Tell me ... is it done?"))

  (:when-talk ((actor &quest the-end-of-evend :done) self topic)
    (tell self actor "I'm glad that's over, and I hope you were able to beat
      some sense into Evend. His father never disciplined him, and once Evend
      was older I dared not, given his dark moods.

      Speaking of my late, no-good husband, he once gave me this ring. I want
      you to take it. It triggers too many sad memories for me, but you may find
      it useful.")
    (receive actor self (list (clone-entity 'mirabels-ring)))
    (advance-quest self actor 'the-end-of-evend)))

;;; spear shop

(defentity miglin (vendor)
  (:brief "Miglin"
   :pose "lounges against the wall."
   :description "Miglin appears to be human, but his features are strangely
     reptilian."
   :sells (copper-spear bronze-spear))

  (:when-talk ((actor &quest talking-shop :active) self topic)
    (tell self actor "Ah, poor Mirabel. She sold a variety of knickknacks, but
      nothing of particular value. To be honest I can't figure why she'd be
      targeted like this.

      Times have been tough since her son, who had been helping with the shop,
      left town. Rumor has it he headed off to a bigger city to seek his
      fortune. Haven't seen him since.")
    (advance-quest self actor 'talking-shop 'spear-vendor))

  (:when-talk (actor self topic)
    (tell self actor "Miglin's my name. I make the best spears in town. True,
      they are the only spears in town, but that doesn't disprove my claim. Type
      `buy` to see what I have for sale.")))

(deflocation spear-shop ()
  (:name "Pointy Sticks, Ltd."
   :description "A variety of spears have been artfully arranged along the
     walls."
   :domain :indoor
   :surface :wood
   :contents (miglin)
   :exits ((exit-doorway :west south-road-2))))

;;; wall street

(defentity monkey (creature)
  (:brief "a costumed monkey"
   :pose "hops from foot to foot."
   :description "The monkey wears a fitted red coat and a yellow cap."
   :move-direction :east)

  (:after-enter-location (self location entry)
    (with-delay (5)
      (show-near self "The monkey dances a lively jig.")
      (with-delay (15)
        (observe-event self :move-along))))

  (:move-along ()
    (let ((exit (find-exit (location self) (? self :move-direction))))
      (when (null exit)
        (setf (? self :move-direction)
              (direction-opposite (? self :move-direction)))
        (setf exit (find-exit (location self) (? self :move-direction))))
      (if exit
        (traverse-portal self (location self) exit)
        (progn
          (show-near self "The monkey scratches itself and sighs.")
          (with-delay (10)
            (observe-event self :move-along))))))

  (:after-emote ((actor &quest dance-monkey :active) message)
    ;; FIXME: message should just be a string
    (when (search "dance" (first message) :test #'char-equal)
      (show-near self "The money claps wildly at ~a's dance moves!"
                      (describe-brief actor))
      (advance-quest self actor 'dance-monkey))))

(defentity wall-street (location)
  (:name "Wall Street"
   :description "This narrow cobbled lane parallels the low wall that marks the
     southern boundary of the village."
   :domain :outdoor
   :surface :stone))

(deflocation wall-street-1 (wall-street)
  (:contents (monkey)
   :exits ((cobbled-road :east wall-street-2)
           (entry-doorway :north lodge-workshop :west front-desk))))

(deflocation wall-street-2 (wall-street)
  (:exits ((cobbled-road :west wall-street-1 :east wall-street-3))))

(defentity wall-street-sign ()
  (:brief "a directional sign"
   :pose "stands at the crossroads."
   :description "Wooden arrows affixed atop the signpost read as follows:

     | North: Village Square
     | West: Armor Emporium
     | South: Perenvale
     | East: Crafters' Hall"))

(deflocation wall-street-3 (wall-street)
  (:contents (wall-street-sign)
   :exits ((cobbled-road :west wall-street-2 :east wall-street-4
                         :south south-gate)
           (dirt-road :north south-road-2))))

(deflocation wall-street-4 (wall-street)
  (:exits ((cobbled-road :west wall-street-3 :east wall-street-5)
           (entry-doorway :south locksmith-shop))))

(deflocation wall-street-5 (wall-street)
  (:exits ((cobbled-road :west wall-street-4 :east wall-street-6))))

(deflocation wall-street-6 (wall-street)
  (:exits ((cobbled-road :west wall-street-5)
           (alley :north muggers-alley-3)
           (entry-doorway :east crafters-hall-1nw))))

;;; armor emporium

(defentity armor-emporium (location)
  (:subregion "Armor Emporium"
   :domain :indoor
   :surface :wood))

(defentity armor-greeter (humanoid)
  (:brief "a heavily-armored man"
   :pose "stands proudly behind a small desk."
   :description "The man wears a mish-mash of various types of armor, including three
     different helms stacked atop one another.")

  (:when-talk (actor self topic)
    (tell self actor "Well met! The vendors beyond can provide you with armor of
      all types. Peruse their wares and be sure to buy something, you'll need
      the protection once you venture outside the town.

      Be aware that armor comes in three basic types: light, medium, and heavy.
      Each type provides more protection than the last, but also incurs more
      penalties in terms of your attack speed. Choose wisely!")))

(deflocation front-desk (armor-emporium)
  (:name "Front Desk"
   :description "A number of battered shields hang on the walls here, displaying
     the coats-of-arms of prominent area families."
   :contents (armor-greeter)
   :exits ((exit-doorway :east wall-street-1)
           (doorway :north medium-armor-shop :west light-armor-shop))))

(defentity lingum (vendor)
  (:name "Lingum"
   :description "Lingum is a goblin, and rather small even for his kind. He
     wears a conical leather hat that has been dyed bright green."
   :sells nil) ; FIXME: thin leather cap tunic gloves leggings boots

  (:when-talk (actor self topic)
    (tell self actor "Greetings, greetings, well met and greetings! Welcome to
      my humble shop, where you can `buy` leather armor in all shapes and sizes.
      Leather is the most versatile of materials and affords much-needed
      protection to an adventurer such as yourself.")))

(deflocation light-armor-shop (armor-emporium)
  (:name "Lingum's Leathers"
   :description "This room smells of tanned animal skin. Leather garments are
     displayed on numerous wicker manneqins."
   :contents (lingum)
   :exits ((doorway :east front-desk :north heavy-armor-shop))
   :tutorial "Lingum sells light armor. It provides less protection than other
     armor types, but more than simple clothing. It has no negative impact on
     your combat actions."))

(defentity chiana (vendor)
  (:name "Chiana"
   :description "Chiana is a white-haired sidhe woman, slim almost to the point
     of gauntness. She wears an exquisite knee-length shirt of gleaming
     chainmail."
   :sells nil) ; FIXME: copper? chain stuff

  (:when-talk (actor self topic)
    (tell self actor "Feel free to peruse my wares. If you choose to `buy` from
      me, you can be assured of the finest quality ... even if you won't
      recognize it.")))

(deflocation medium-armor-shop (armor-emporium)
  (:name "Chiana's Chainmail"
   :contents (chiana)
   :exits ((doorway :south front-desk :west heavy-armor-shop))
   :tutorial "Chiana sells medium armor. It provides more protection than light
     armor but less than heavy armor. It slows your combat actions slightly."))

(defentity palla (vendor)
  (:name "Palla"
   :description "Palla is a dwarven woman. She wears a gaily-colored cotton
     dress and her waist in girded with a belt of polished steel plates."
   :sells nil) ; FIXME: copper? plate stuff

  (:when-talk (actor self topic)
    (tell self actor "Plate armor provides the most protection, it's true! But
      it is far too heavy to wear all day while tending my shop.")))

(deflocation heavy-armor-shop (armor-emporium)
  (:name "Palla's Platemail"
   :contents (palla)
   :exits ((doorway :south light-armor-shop :east medium-armor-shop))
   :tutorial "The heavy armor sold here provides the most protection, but also
     slows you down considerably in combat."))

;;; FIXME: crafters' hall

;;; south gate

(defentity south-guard (guard)
  (:pose "stands at attention, spear in hand.")

  (:when-talk ((actor &quest at-the-gates :active) self topic)
    (tell self actor "Ah, that boy was always a pain in my arse. I can't blame
      his mother. She's a good woman and did her best. Have I seen him lately?
      No, he hasn't passed through my gate, that's for certain.")
    (advance-quest self actor 'at-the-gates 'south-gate))

  (:when-talk (actor self topic)
    (tell self actor "To the south you'll find Perenvale. It's a nice place to
      visit as long as you avoid the bloodthirsty centaurs that roam the plains
      between here and there.")))

(deflocation south-gate ()
  (:name "South Gate"
   :domain :outdoor
   :surface :stone
   :contents (south-guard)
   :exits ((cobbled-road :north wall-street-3))))

;;; locksmith shop

(defentity pewter-key (item)
  (:brief "a tiny pewter key"
   :description "Kijian the locksmith assures you this key will open the door to Evend's
     hideout."
   :icon silver-key
   :bound t))

(defentity locksmith (humanoid)
  (:name "Kijian"
   :description "Kijian is a human male of medium height. His steel gray hair is
     pulled back in a short ponytail. His impressive mustache is waxed and
     curled.")

  (:when-talk ((actor &quest the-key-is-the-key :find-kijian) self topic)
    (show actor "You explain the situation to Kijian.")
    (with-delay (2)
      (tell self actor "So Mirabel needs to unlock a door, eh? Well of course
        I'm happy to help. I can make a key to open that lock, but while I work
        I'll need you to do something for me.

        To the west of town, in Silverwood, you can find giant spiders. I need
        one of their silky spiderwebs for a side project. Don't ask.

        Also, just north of the forest, there's an abandoned mine that's
        infested with kobolds. I want to study one of their trinkets.

        Bring me both of those items, and I'll give you the key.")
      (advance-quest self actor 'the-key-is-the-key)))

  (:when-talk ((actor &quest the-key-is-the-key :gather-items) self topic)
    (tell self actor "Did you get the spiderweb and kobold trinket? Hand 'em
      over and you'll get your key."))

  (:when-give ((actor &quest the-key-is-the-key :gather-items)
               (item jade.silverwood::silky-spiderweb) self)
    (tell self actor "Ho ho, that really *is* quite silky!")
    (advance-quest self actor 'the-key-is-the-key 'silky-spiderweb))

  (:when-give ((actor &quest the-key-is-the-key :gather-items)
               (item jade.copper-mine::kobold-trinket) self)
    (tell self actor "Ha ha, that isn't quite what I imagined, but still very
      interesting!")
    (advance-quest self actor 'the-key-is-the-key 'kobold-trinket))

  (:when-talk ((actor &quest the-key-is-the-key :key-ready) self topic)
    (receive actor self (list (clone-entity 'pewter-key)))
    (tell self actor "There's your key. Tell Mirabel I wish her all the best.")
    (advance-quest self actor 'the-key-is-the-key)))

(deflocation locksmith-shop ()
  (:name "Kijian's Locks and Keys"
   :description "This shop is empty but for a battered workbench and a few crates stacked
     in the corner."
   :domain :indoor
   :surface :wood
   :contents (locksmith)
   :exits ((exit-doorway :north wall-street-4))))

;;; forest road

(defentity forest-road (location)
  (:name "Forest Road"
   :description "This narrow dirt road runs between the village and a small forest to
     the west."
   :domain :outdoor
   :surface :dirt))

(deflocation forest-road-1 (forest-road)
  (:exits ((dirt-road :east square-sw :west forest-road-2))))

(defentity shack-door () ; FIXME: (portal)?
  (:key pewter-key))

(deflocation forest-road-2 (forest-road)
  (:exits ((dirt-road :east forest-road-1 :west forest-road-3)
           (shack-door :south shack))))

(deflocation forest-road-3 (forest-road)
  (:exits ((dirt-road :east forest-road-2 :west forest-gate)
           (narrow-path :north grove-sw))))

;;; shack

(defentity gang-member (humanoid)
  (:min-health 10)

  (:after-kill (actor self)
    (show-near self "~a falls to his knees in submission."
               (describe-brief self :capitalize t))))

(defentity goon (gang-member)
  (:brief "a barrel-chested goon"
   :attacks (copper-mace)))

(defentity ruffian (gang-member)
  (:brief "a lanky ruffian"
   :attacks (copper-dagger)))

(defentity evend (gang-member)
  (:name "Evend"
   :attacks (copper-sword))

  ;; TODO: how to advance quest after all three are subdued? then reset them.

  (:after-enter-location (actor location entry)
    (tell self actor "Hey! How did you get in here? You'd better turn around and
      leave, or my friends here will make you sorry.")))

#|
  after kill(actor, self, weapon)
    showNear(self) "self:D) falls to his knees in submission."
    if advanceQuest(actor, theEndOfEvend, self.questKey)
      tell(evend, actor) |
        I don't know how, but you've beaten us all. I guess we're not
        strong enough to run this town. For now.
      show(actor) |
        Evend motions to his henchmen, and all three of them stumble out
        of the shack.
)
|#

(deflocation shack ()
  (:name "Evend's Hideout"
   :description "Despite the ramshackle appearance of its exterior, the interior
     of this shack is quite comfortable."
   :domain :indoor
   :surface :wood
   :contents (evend goon ruffian)
   :exits ((exit-doorway :north forest-road-2))))

;;; secluded grove

(defentity grove (location)
  (:name "Secluded Grove"
   :description "This beautiful grove of aspens and birches seems unnaturally
     quiet, especially given the proximity of the village."
   :domain :outdoor
   :surface :forest))

(deflocation grove-sw (grove)
  (:exits ((narrow-path :south forest-road-3 :north grove-nw :east grove-se))))

(defentity raspin (humanoid) ; FIXME: (druid-trainer)
  (:name "Raspin Redleaf"
   :pose "tends to a nearby tree."
   :description "Raspin is a wiry, rugged-looking man who wears plain brown
     clothes well suited to travel in the wilderness. His wide leather belt
     holds numerous small pouches as well as a pair of long, curved daggers.")

  (:when-talk (actor self topic)
    (tell self actor "Welcome, friend. I am Raspin, a representative of the
      Circle of the Grove.

      My guild teaches skills that draw upon the power of nature to heal,
      sustain, and --- when needed --- destroy. Type `learn` to see what I can
      teach you, or type `help skills` for more general information.")))

(deflocation grove-nw (grove)
  (:contents (raspin)
   :exits ((narrow-path :south grove-sw :east grove-ne))))

(deflocation grove-ne (grove)
  (:exits ((narrow-path :west grove-nw :south grove-se :north tidy-garden))))

(defentity ilivrian (vendor)
  (:name "Ilivrian"
   :pose "polishes one of the many staves on display."
   :description "Ilivrian is a very tall elf with long golden hair."
   :sells (pine-staff oak-staff))

  (:when-talk (actor self topic)
    (tell self actor "Welcome, traveler. Are you in need of a staff, perchance?
      Type `buy` to see what I have available.")))

(deflocation grove-se (grove)
  (:contents (ilivrian)
   :exits ((narrow-path :north grove-ne :west grove-sw))))

;;; garden

(defentity gariande (humanoid) ; FIXME: (botany-trainer)
  (:name "Gariande"
   :pose "is busily tying up bundles of herbs."
   :description "Gariande is an very old woman with deep wrinkles on her face. Her
     steel-gray hair is tied in a bun."
   :teaches nil) ; FIXME: from botany-trainer

  (:when-talk (actor self topic)
    (tell self actor "Come for some sweet corn? Or perhaps a few tomatoes? Well,
      you're out of luck: they're not for sale. But if you want to `learn` about
      botany, then you've come to the right place. I represent the Botanists'
      Guild in these parts. Type `guild info` to learn more.")))

(defentity bonnos (vendor)
  (:name "Bonnos"
   :pose "is watering the plants."
   :description "Bonnos is a man of perhaps thirty years, with perhaps ten
      years' worth of dirt under his fingernails."
   :sells nil) ; FIXME: copper and bronze sickles

  (:when-talk (actor self topic)
    (tell self actor "Nice to meet you. Grandmother keeps me quite busy here, so
      I don't meet many travelers. If you've come to explore botany, you can
      `buy` the tools you'll need from me.")))

(deflocation tidy-garden ()
  (:name "Tidy Garden"
   :description "This small plot is filled with rows of carefully-tended
      vegetables and herbs."
   :domain :outdoor
   :surface :grass
   :contents (gariande bonnos)
   :exits ((narrow-path :south grove-ne))))

;;; forest gate

(defquest find-my-son
  (:name "Find My Son"
   :summary "Find Miranda's son, who disappeared with his friend while picking
     berries in Silverwood, and let her know if he's ok."
   :level 2)

  (:active
    :summary "Find Miranda's soon in the Silverwood.")

  (:done
    :summary "Report to Miranda."))

(defentity miranda (humanoid)
  (:name "Miranda Mathers"
   :description "Miranda is a stout middle-aged woman. She wears practical
     clothing that has seen numerous repairs over the years."
   :offers-quests (find-my-son))

  (:when-talk ((actor &quest find-my-son :available) self topic)
    (tell self actor "I know we've never met, but I have nobody else to turn to.
      My son is missing. Will you help me?")
    (offer-quest self 'find-my-son actor))

  (:after-accept-quest (actor (quest &quest find-my-son) self)
    (tell self actor "Oh, thank the gods! My son went into the woods west of
      here to pick some berries with his friend. He has been there dozens of
      times without any problems, but today he never returned. I'm so worried!

      Please ... find my boy. I don't know what I'd do if I lost him."))

  (:when-talk ((actor &quest find-my-son :active) self topic)
    (tell self actor "Have you found my boy? He should be in the forest to the
      west."))

  (:when-talk ((actor &quest find-my-son :done) self topic)
    (tell self actor "Thank the gods you found him! He may be a little rascal,
      but he's *my* little rascal and I love him to death.")
    (advance-quest self actor 'find-my-son))

  (:when-talk (actor self topic)
    (tell self actor "A pleasant day to you!")))

(defentity rhody (humanoid)
  (:name "Rhody Mathers"
   :pose "fidgets nearby."
   :description "Rhody is a young boy, perhaps eight years old. Every now and
     then you catch him glancing westward, toward the forest.")

  ;; FIXME: visible only if find-my-son is :done or :finished

  (:when-talk (actor self topic)
    (tell self actor "Hello again! Don't tell my mom, but as soon as her back is
      turned I'm gonna sneak into the forest for more berries!")))

#|

  // FIXME: move this to the Rhody in the forest.
  when talk(actor: .quest(findMySon, active), self, topic)
    tell(self, actor) |
      Hi! Do I know you? Oh, my mom sent you? I'm sorry she got so
      worried. I guess I should head back. Nice to meet you!
    show(avatar) |
      Rhody wipes his face with his sleeve then heads north, back toward
      Arwyck.
    advanceQuest(actor, findMySon)
)

|#

(defentity forest-guard (guard)
  (:pose "keeps an eye on the road.")

  (:when-talk ((actor &quest at-the-gates :active) self topic)
    (tell self actor "Mirabel's son?")
    (show actor "The guard spits into the dirt.")
    (tell self actor "I know of him. Always a troublemaker. But I haven't seen
      him in ages, thank the gods.")
    (advance-quest self actor 'at-the-gates 'west-gate))

  (:when-talk (actor self topic)
    (tell self actor "Be wary in the forest to the west, traveler. Those spiders
    are no joke.")))

(deflocation forest-gate ()
  (:name "Forest Gate"
   :description "A stout wooden stockade separates Arwyck from the forest to the
     west. A narrow gate allows access to the wilderness beyond."
   :domain :outdoor
   :surface :dirt
   :contents (miranda rhody forest-guard)
   :exits ((dirt-road :east forest-road-3 :west jade.silverwood::road-z14))))
