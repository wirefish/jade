(in-package :jade.silverwood)

(defentity silverwood ()
  (:name "Silverwood"
   :description "The Silverwood is a small forest nestled in the hills west of Arwyck."
   :climate :temperate
   :level-range (1 10)))

#|
    A B C D E F G H I J K L M N O P Q R S T

00          :-:-:-:       :-:-:-:-S-:-:
            | | | |       | | | | | | |
01      :-:-:-:-:-:-:   :-:-:-:-:-S-:-:-:
        | | | | | | |   | | | | | | | | |
02    :-:-:-:-:-:-:-:-:-:-:-:-:-:-S-:-:-:-:
      | | | | | | | | | | | | | | | | | | |
03  :-:-:-:-:-:-:-:-:-:-:-:-:-:-:-S-:-:-:-:
    | | | | | | | | | | | | | | | | | | | |
04  :-:-:-C-C-C-:-:-:-:-:-:-:-:-:-S-:-:-:-:
    | | | | | | | | | | | | | | |   | | | |
05  :-:-:-C-C-C-:-:-:-:-:-:-:-R-R-=-R-R-R-R
    | | | | | | | | | | | | | | |   | | | |
06  :-:-:-C-C-C-:-:-:-:-:-:-:-R-:-S-:-:-:-:
    | | | | | | | | | | | | | | | | | | | |
07  :-:-:-:-:-:-:-:-:-:-:-:-:-R-:-S-:-:-:-:
    | | | | | | | | | | | | | | | | | | |
08  :-:-:-:-:-:-:-:-:-:-:-:-:-R-:-S-:-:-:
    | | | | | | | | | | | | | | | | | |
09  :-:-:-:-:-:-:-:-:-:-:-:-:-R-:-S-:-:
          | | | | | | | | | | | | | | |
10    :-:-:-:-:-:-:-:-:-:-:-:-R-:-S-:-:-:
     /      | | | | | | | | | | | | | | |
11  :       :-:-:-:-:-:-:-:-:-R-:-S-:-:-:-:
            | | | | | | | | | | | | | | | |
12      :-:-:-:-:-:-:-:-R-R-R-R-:-S :-:-:-:
        | | | | | | | | | | | | | |   | | |
13  :-:-:-:-:-:-:-:-:-:-R-:-:-X-:-S-S-S-:-:
    | | | | | | | | | | | | | | | | | | | |
14  :-:-:-:-:-:-:-:-R-R-R-:-:-:-:-:-: c :-:
    | | | | | | | | | | | | | | |     |
15  :-:-:-:-:-:-:-:-R-:-:-:-:-:-:     c
      | | | | | | | | | | | | | |     |
16    :-:-:-:-:-:-:-R-:-:-:-:-:-:     c
        | | | | | | | | | | |    \    |
17      :-:-:-:-:-:-R-:-:-:-:     :-: c

|#

;;; spiders

(defentity giant-spider-bite (bite)
  (:speed 2.5
   :base-damage 4))

(defentity spider-silk (item)
  (:brief "a clump of spider silk"
   :description "This item can be turned into thread by a skilled weaver."
   :icon spider-web
   :level 1
   :size +tiny+
   :stackable t))

(defentity giant-spider (combatant)
  (:brief "a giant forest spider"
   :pose "hangs from a nearby branch."
   :description "This enormous webspinner has luminous eyes and long, hairy
     legs. Its kind prefer to lurk in the forest canopy, awaiting the
     opportunity to drop down upon unsuspecting prey."
   :icon long-legged-spider
   :level 1
   :attacks (giant-spider-bite)
   :entry-message "drops down from the branches above."
   :loot ((0.5 spider-silk))))

(limit-spawn-quantity 'giant-spider 25)

;;; mushroom clusters

(defentity small-brown-mushroom (botany-resource)
  (:brief "a small brown mushroom"
   :icon mushroom-6
   :required-rank 1))

(defentity small-white-mushroom (botany-resource)
  (:brief "a small white mushroom"
   :icon mushroom-3
   :required-rank 20))

(defentity small-speckled-mushroom (botany-resource)
  (:brief "a small speckled mushroom"
   :icon mushroom-1
   :required-rank 40))

(defentity mushroom-cluster (botany-node)
  (:brief "a cluster of small mushrooms"
   :icon mushroom-6
   :resources ((1.0 small-brown-mushroom :quantity (random-integer 1 2))
               (0.2 small-white-mushroom)
               (0.01 small-speckled-mushroom))))

(limit-spawn-quantity 'mushroom-cluster 25)

;;; maple trees

(defentity maple-log (logging-resource)
  (:brief "a maple log"
   :required-rank 1))

(defentity maple-tree (resource-node)
  (:brief "a mature maple tree"
   :pose "stands nearby."
   :required-skill logging
   :required-rank 1
   :resources ((1.0 maple-log :quantity (random-integer 1 2)))))

(limit-spawn-quantity 'maple-tree 20)

;;; birch trees

(defentity birch-log (logging-resource)
  (:brief "a birch log"
   :required-rank 20))

(defentity birch-tree (resource-node)
  (:brief "a mature birch tree"
   :pose "stands nearby."
   :required-skill logging
   :required-rank 20
   :resources ((1.0 birch-log :quantity (random-integer 1 2)))))

(limit-spawn-quantity 'birch-tree 5)

;;; portal prototypes

(defentity forest-portal (continuing-portal)
  (:brief "the forest"))

(defentity road-portal (continuing-portal)
  (:brief "the road"))

(defentity clearing-portal (continuing-portal)
  (:brief "the clearing"))

(defentity stream-portal (continuing-portal)
  (:brief "the stream"))

(defentity canyon-portal (continuing-portal)
  (:brief "the canyon"))

(defentity narrow-track ()
  (:brief "a barely-visible track"))

;;; forest

(defentity forest (location)
  (:name "Forest"
   :description "You are surrounded by a beautiful forest of birch and maple
     trees."
   :domain :outdoor
   :surface :forest)

  (:after-enter-world ()
    (with-random-interval (10 120)
      (whichever
       (spawn-entity self 'mushroom-cluster)
       (spawn-entity self 'maple-tree)
       (spawn-entity self 'birch-tree)
       (spawn-entity self 'large-spiderweb)
       (spawn-entity self 'giant-spider)))))

(deflocation forest-E00 (forest)
  (:exits ((forest-portal :south forest-E01 :east forest-F00))))

(deflocation forest-F00 (forest)
  (:exits ((forest-portal :west forest-E00 :south forest-F01 :east forest-G00)
           (narrow-track :north jade.copper-mine::entrance))))

(deflocation forest-G00 (forest)
  (:exits ((forest-portal :west forest-F00 :south forest-G01 :east forest-H00))))

(deflocation forest-H00 (forest)
  (:exits ((forest-portal :west forest-G00 :south forest-H01))))

(deflocation forest-L00 (forest)
  (:exits ((forest-portal :south forest-L01 :east forest-M00))))

(deflocation forest-M00 (forest)
  (:exits ((forest-portal :west forest-L00 :south forest-M01 :east forest-N00))))

(deflocation forest-N00 (forest)
  (:exits ((forest-portal :west forest-M00 :south forest-N01 :east forest-O00))))

(deflocation forest-O00 (forest)
  (:exits ((forest-portal :west forest-N00 :south forest-O01 :east forest-stream-P00))))

(deflocation forest-Q00 (forest)
  (:exits ((forest-portal :west forest-stream-P00 :south forest-Q01 :east forest-R00))))

(deflocation forest-R00 (forest)
  (:exits ((forest-portal :west forest-Q00 :south forest-R01))))

(deflocation forest-C01 (forest)
  (:exits ((forest-portal :south forest-C02 :east forest-D01))))

(deflocation forest-D01 (forest)
  (:exits ((forest-portal :west forest-C01 :south forest-D02 :east forest-E01))))

(deflocation forest-E01 (forest)
  (:exits ((forest-portal :west forest-D01 :north forest-E00 :south forest-E02
                          :east forest-F01))))

(deflocation forest-F01 (forest)
  (:exits ((forest-portal :west forest-E01 :north forest-F00 :south forest-F02
                          :east forest-G01))))

(deflocation forest-G01 (forest)
  (:exits ((forest-portal :west forest-F01 :north forest-G00 :south forest-G02
                          :east forest-H01))))

(deflocation forest-H01 (forest)
  (:exits ((forest-portal :west forest-G01 :north forest-H00 :south forest-H02
                          :east forest-I01))))

(deflocation forest-I01 (forest)
  (:exits ((forest-portal :west forest-H01 :south forest-I02))))

(deflocation forest-K01 (forest)
  (:exits ((forest-portal :south forest-K02 :east forest-L01))))

(deflocation forest-L01 (forest)
  (:exits ((forest-portal :west forest-K01 :north forest-L00 :south forest-L02
                          :east forest-M01))))

(deflocation forest-M01 (forest)
  (:exits ((forest-portal :west forest-L01 :north forest-M00 :south forest-M02
                          :east forest-N01))))

(deflocation forest-N01 (forest)
  (:exits ((forest-portal :west forest-M01 :north forest-N00 :south forest-N02
                          :east forest-O01))))

(deflocation forest-O01 (forest)
  (:exits ((forest-portal :west forest-N01 :north forest-O00 :south forest-O02
                          :east forest-stream-P01))))

(deflocation forest-Q01 (forest)
  (:exits ((forest-portal :west forest-stream-P01 :north forest-Q00
                          :south forest-Q02 :east forest-R01))))

(deflocation forest-R01 (forest)
  (:exits ((forest-portal :west forest-Q01 :north forest-R00 :south forest-R02
                          :east forest-S01))))

(deflocation forest-S01 (forest)
  (:exits ((forest-portal :west forest-R01 :south forest-S02))))

(deflocation forest-B02 (forest)
  (:exits ((forest-portal :south forest-B03 :east forest-C02))))

(deflocation forest-C02 (forest)
  (:exits ((forest-portal :west forest-B02 :north forest-C01 :south forest-C03
                          :east forest-D02))))

(deflocation forest-D02 (forest)
  (:exits ((forest-portal :west forest-C02 :north forest-D01 :south forest-D03
                          :east forest-E02))))

(deflocation forest-E02 (forest)
  (:exits ((forest-portal :west forest-D02 :north forest-E01 :south forest-E03
                          :east forest-F02))))

(deflocation forest-F02 (forest)
  (:exits ((forest-portal :west forest-E02 :north forest-F01 :south forest-F03
                          :east forest-G02))))

(deflocation forest-G02 (forest)
  (:exits ((forest-portal :west forest-F02 :north forest-G01 :south forest-G03
                          :east forest-H02))))

(deflocation forest-H02 (forest)
  (:exits ((forest-portal :west forest-G02 :north forest-H01 :south forest-H03
                          :east forest-I02))))

(deflocation forest-I02 (forest)
  (:exits ((forest-portal :west forest-H02 :north forest-I01 :south forest-I03
                          :east forest-J02))))

(deflocation forest-J02 (forest)
  (:exits ((forest-portal :west forest-I02 :south forest-J03 :east forest-K02))))

(deflocation forest-K02 (forest)
  (:exits ((forest-portal :west forest-J02 :north forest-K01 :south forest-K03
                          :east forest-L02))))

(deflocation forest-L02 (forest)
  (:exits ((forest-portal :west forest-K02 :north forest-L01 :south forest-L03
                          :east forest-M02))))

(deflocation forest-M02 (forest)
  (:exits ((forest-portal :west forest-L02 :north forest-M01 :south forest-M03
                          :east forest-N02))))

(deflocation forest-N02 (forest)
  (:exits ((forest-portal :west forest-M02 :north forest-N01 :south forest-N03
                          :east forest-O02))))

(deflocation forest-O02 (forest)
  (:exits ((forest-portal :west forest-N02 :north forest-O01 :south forest-O03
                          :east forest-stream-P02))))

(deflocation forest-Q02 (forest)
  (:exits ((forest-portal :west forest-stream-P02 :north forest-Q01
                          :south forest-Q03 :east forest-R02))))

(deflocation forest-R02 (forest)
  (:exits ((forest-portal :west forest-Q02 :north forest-R01 :south forest-R03
                          :east forest-S02))))

(deflocation forest-S02 (forest)
  (:exits ((forest-portal :west forest-R02 :north forest-S01 :south forest-S03
                          :east forest-T02))))

(deflocation forest-T02 (forest)
  (:exits ((forest-portal :west forest-S02 :south forest-T03))))

(deflocation forest-A03 (forest)
  (:exits ((forest-portal :south forest-A04 :east forest-B03))))

(deflocation forest-B03 (forest)
  (:exits ((forest-portal :west forest-A03 :north forest-B02 :south forest-B04
                          :east forest-C03))))

(deflocation forest-C03 (forest)
  (:exits ((forest-portal :west forest-B03 :north forest-C02 :south forest-C04
                          :east forest-D03))))

(deflocation forest-D03 (forest)
  (:exits ((forest-portal :west forest-C03 :north forest-D02 :south clearing-D04
                          :east forest-E03))))

(deflocation forest-E03 (forest)
  (:exits ((forest-portal :west forest-D03 :north forest-E02 :south clearing-E04
                          :east forest-F03))))

(deflocation forest-F03 (forest)
  (:exits ((forest-portal :west forest-E03 :north forest-F02 :south clearing-F04
                          :east forest-G03))))

(deflocation forest-G03 (forest)
  (:exits ((forest-portal :west forest-F03 :north forest-G02 :south forest-G04
                          :east forest-H03))))

(deflocation forest-H03 (forest)
  (:exits ((forest-portal :west forest-G03 :north forest-H02 :south forest-H04
                          :east forest-I03))))

(deflocation forest-I03 (forest)
  (:exits ((forest-portal :west forest-H03 :north forest-I02 :south forest-I04
                          :east forest-J03))))

(deflocation forest-J03 (forest)
  (:exits ((forest-portal :west forest-I03 :north forest-J02 :south forest-J04
                          :east forest-K03))))

(deflocation forest-K03 (forest)
  (:exits ((forest-portal :west forest-J03 :north forest-K02 :south forest-K04
                          :east forest-L03))))

(deflocation forest-L03 (forest)
  (:exits ((forest-portal :west forest-K03 :north forest-L02 :south forest-L04
                          :east forest-M03))))

(deflocation forest-M03 (forest)
  (:exits ((forest-portal :west forest-L03 :north forest-M02 :south forest-M04
                          :east forest-N03))))

(deflocation forest-N03 (forest)
  (:exits ((forest-portal :west forest-M03 :north forest-N02 :south forest-N04
                          :east forest-O03))))

(deflocation forest-O03 (forest)
  (:exits ((forest-portal :west forest-N03 :north forest-O02 :south forest-O04
                          :east forest-stream-P03))))

(deflocation forest-Q03 (forest)
  (:exits ((forest-portal :west forest-stream-P03 :north forest-Q02
                          :south forest-Q04 :east forest-R03))))

(deflocation forest-R03 (forest)
  (:exits ((forest-portal :west forest-Q03 :north forest-R02 :south forest-R04
                          :east forest-S03))))

(deflocation forest-S03 (forest)
  (:exits ((forest-portal :west forest-R03 :north forest-S02 :south forest-S04
                          :east forest-T03))))

(deflocation forest-T03 (forest)
  (:exits ((forest-portal :west forest-S03 :north forest-T02 :south forest-T04))))

(deflocation forest-A04 (forest)
  (:exits ((forest-portal :north forest-A03 :south forest-A05 :east forest-B04))))

(deflocation forest-B04 (forest)
  (:exits ((forest-portal :west forest-A04 :north forest-B03 :south forest-B05
                          :east forest-C04))))

(deflocation forest-C04 (forest)
  (:exits ((forest-portal :west forest-B04 :north forest-C03 :south forest-C05
                          :east clearing-D04))))

(deflocation forest-G04 (forest)
  (:exits ((forest-portal :west clearing-F04 :north forest-G03 :south forest-G05
                          :east forest-H04))))

(deflocation forest-H04 (forest)
  (:exits ((forest-portal :west forest-G04 :north forest-H03 :south forest-H05
                          :east forest-I04))))

(deflocation forest-I04 (forest)
  (:exits ((forest-portal :west forest-H04 :north forest-I03 :south forest-I05
                          :east forest-J04))))

(deflocation forest-J04 (forest)
  (:exits ((forest-portal :west forest-I04 :north forest-J03 :south forest-J05
                          :east forest-K04))))

(deflocation forest-K04 (forest)
  (:exits ((forest-portal :west forest-J04 :north forest-K03 :south forest-K05
                          :east forest-L04))))

(deflocation forest-L04 (forest)
  (:exits ((forest-portal :west forest-K04 :north forest-L03 :south forest-L05
                          :east forest-M04))))

(deflocation forest-M04 (forest)
  (:exits ((forest-portal :west forest-L04 :north forest-M03 :south forest-M05
                          :east forest-N04))))

(deflocation forest-N04 (forest)
  (:exits ((forest-portal :west forest-M04 :north forest-N03 :south road-N05
                          :east forest-O04))))

(deflocation forest-O04 (forest)
  (:exits ((forest-portal :west forest-N04 :north forest-O03 :south road-O05
                          :east forest-stream-P04))))

(deflocation forest-Q04 (forest)
  (:exits ((forest-portal :west forest-stream-P04 :north forest-Q03 :south road-Q05
                          :east forest-R04))))

(deflocation forest-R04 (forest)
  (:exits ((forest-portal :west forest-Q04 :north forest-R03 :south road-R05
                          :east forest-S04))))

(deflocation forest-S04 (forest)
  (:exits ((forest-portal :west forest-R04 :north forest-S03 :south road-S05
                          :east forest-T04))))

(deflocation forest-T04 (forest)
  (:exits ((forest-portal :west forest-S04 :north forest-T03 :south road-T05))))

(deflocation forest-A05 (forest)
  (:exits ((forest-portal :north forest-A04 :south forest-A06 :east forest-B05))))

(deflocation forest-B05 (forest)
  (:exits ((forest-portal :west forest-A05 :north forest-B04 :south forest-B06
                          :east forest-C05))))

(deflocation forest-C05 (forest)
  (:exits ((forest-portal :west forest-B05 :north forest-C04 :south forest-C06
                          :east clearing-D05))))

(deflocation forest-G05 (forest)
  (:exits ((forest-portal :west clearing-F05 :north forest-G04 :south forest-G06
                          :east forest-H05))))

(deflocation forest-H05 (forest)
  (:exits ((forest-portal :west forest-G05 :north forest-H04 :south forest-H06
                          :east forest-I05))))

(deflocation forest-I05 (forest)
  (:exits ((forest-portal :west forest-H05 :north forest-I04 :south forest-I06
                          :east forest-J05))))

(deflocation forest-J05 (forest)
  (:exits ((forest-portal :west forest-I05 :north forest-J04 :south forest-J06
                          :east forest-K05))))

(deflocation forest-K05 (forest)
  (:exits ((forest-portal :west forest-J05 :north forest-K04 :south forest-K06
                          :east forest-L05))))

(deflocation forest-L05 (forest)
  (:exits ((forest-portal :west forest-K05 :north forest-L04 :south forest-L06
                          :east forest-M05))))

(deflocation forest-M05 (forest)
  (:exits ((forest-portal :west forest-L05 :north forest-M04 :south forest-M06
                          :east road-N05))))

(deflocation forest-A06 (forest)
  (:exits ((forest-portal :north forest-A05 :south forest-A07 :east forest-B06))))

(deflocation forest-B06 (forest)
  (:exits ((forest-portal :west forest-A06 :north forest-B05 :south forest-B07
                          :east forest-C06))))

(deflocation forest-C06 (forest)
  (:exits ((forest-portal :west forest-B06 :north forest-C05 :south forest-C07
                          :east clearing-D06))))

(deflocation forest-G06 (forest)
  (:exits ((forest-portal :west clearing-F06 :north forest-G05 :south forest-G07
                          :east forest-H06))))

(deflocation forest-H06 (forest)
  (:exits ((forest-portal :west forest-G06 :north forest-H05 :south forest-H07
                          :east forest-I06))))

(deflocation forest-I06 (forest)
  (:exits ((forest-portal :west forest-H06 :north forest-I05 :south forest-I07
                          :east forest-J06))))

(deflocation forest-J06 (forest)
  (:exits ((forest-portal :west forest-I06 :north forest-J05 :south forest-J07
                          :east forest-K06))))

(deflocation forest-K06 (forest)
  (:exits ((forest-portal :west forest-J06 :north forest-K05 :south forest-K07
                          :east forest-L06))))

(deflocation forest-L06 (forest)
  (:exits ((forest-portal :west forest-K06 :north forest-L05 :south forest-L07
                          :east forest-M06))))

(deflocation forest-M06 (forest)
  (:exits ((forest-portal :west forest-L06 :north forest-M05 :south forest-M07
                          :east road-N06))))

(deflocation forest-O06 (forest)
  (:exits ((forest-portal :west road-N06 :north road-O05 :south forest-O07
                          :east forest-stream-P06))))

(defentity mill-exterior ()
  (:brief "a water mill"
   :pose "stands on the bank of the stream, its wheel spinning slowly."
   :description "The mill is made of rough-hewn maple logs, with a roof of
     orange clay tiles."
   :implicit-neighbor t))

(defentity jopalinson (logging-trainer)
  (:name "Jopalinson"
   :pose "sorts through a pile of logs in the corner."
   :description "Jopalinson's a lumberjack and he's ok, he sleeps all night and
     he works all day.")

  (:after-enter-world ()
    (with-random-interval (40 60)
      (say self
           (whichever
            "I cut down trees, I eat my lunch, I go to the lavatory!"
            "On Wednesdays I go shoppin' and have buttered scones for tea!"
            "I cut down trees, I skip and jump, I like to press wild flowers!"
            "I put on women's clothing and hang around in bars!"
            "I cut down trees, I wear high heels, suspendies, and a bra!"
            "I wish I'd been a girlie just like my dear papa!"))))

  (:when-talk (actor self topic)
    (tell self actor "Come to learn about logging? Perhaps you seek to one day
      cut down a towering Wattle of Aldershot? I can teach you all about it! Type
      `learn` to learn more.")))

(defentity termired (vendor)
  (:name "Termired"
   :pose "leans against the wall."
   :description "Termired is Jopalinson's best buddy."
   :sells (copper-logging-axe bronze-logging-axe))

  (:when-talk (actor self topic)
    (tell self actor "I have just the axe you'll need to cut down a flatulent
      Elm of West Ruislip, should you discover such a legendary tree. Type `buy`
      to see my wares.")))

(deflocation water-mill ()
  (:name "Water Mill"
   :description "The water wheel outside this building powers an ingenious
     sawmill that lets a skilled operator quickly turn logs into rough-cut
     lumber."
   :domain :indoor
   :surface :wood
   :contents (jopalinson termired)
   :exits ((exit-doorway :out forest-Q06))))

(deflocation forest-Q06 (forest)
  (:icon mill
   :exits ((forest-portal :west forest-stream-P06 :north road-Q05 :south forest-Q07
                          :east forest-R06)
           (entry-doorway :in water-mill))))

(deflocation forest-R06 (forest)
  (:exits ((forest-portal :west forest-Q06 :north road-R05 :south forest-R07
                          :east forest-S06))))

(deflocation forest-S06 (forest)
  (:exits ((forest-portal :west forest-R06 :north road-S05 :south forest-S07
                          :east forest-T06))))

(deflocation forest-T06 (forest)
  (:exits ((forest-portal :west forest-S06 :north road-T05 :south forest-T07))))

(deflocation forest-A07 (forest)
  (:exits ((forest-portal :north forest-A06 :south forest-A08 :east forest-B07))))

(deflocation forest-B07 (forest)
  (:exits ((forest-portal :west forest-A07 :north forest-B06 :south forest-B08
                          :east forest-C07))))

(deflocation forest-C07 (forest)
  (:exits ((forest-portal :west forest-B07 :north forest-C06 :south forest-C08
                          :east forest-D07))))

(deflocation forest-D07 (forest)
  (:exits ((forest-portal :west forest-C07 :north clearing-D06 :south forest-D08
                          :east forest-E07))))

(deflocation forest-E07 (forest)
  (:exits ((forest-portal :west forest-D07 :north clearing-E06 :south forest-E08
                          :east forest-F07))))

(deflocation forest-F07 (forest)
  (:exits ((forest-portal :west forest-E07 :north clearing-F06 :south forest-F08
                          :east forest-G07))))

(deflocation forest-G07 (forest)
  (:exits ((forest-portal :west forest-F07 :north forest-G06 :south forest-G08
                          :east forest-H07))))

(deflocation forest-H07 (forest)
  (:exits ((forest-portal :west forest-G07 :north forest-H06 :south forest-H08
                          :east forest-I07))))

(deflocation forest-I07 (forest)
  (:exits ((forest-portal :west forest-H07 :north forest-I06 :south forest-I08
                          :east forest-J07))))

(deflocation forest-J07 (forest)
  (:exits ((forest-portal :west forest-I07 :north forest-J06 :south forest-J08
                          :east forest-K07))))

(deflocation forest-K07 (forest)
  (:exits ((forest-portal :west forest-J07 :north forest-K06 :south forest-K08
                          :east forest-L07))))

(deflocation forest-L07 (forest)
  (:exits ((forest-portal :west forest-K07 :north forest-L06 :south forest-L08
                          :east forest-M07))))

(deflocation forest-M07 (forest)
  (:exits ((forest-portal :west forest-L07 :north forest-M06 :south forest-M08
                          :east road-N07))))

(deflocation forest-O07 (forest)
  (:exits ((forest-portal :west road-N07 :north forest-O06 :south forest-O08
                          :east forest-stream-P07))))

(deflocation forest-Q07 (forest)
  (:exits ((forest-portal :west forest-stream-P07 :north forest-Q06
                          :south forest-Q08 :east forest-R07))))

(deflocation forest-R07 (forest)
  (:exits ((forest-portal :west forest-Q07 :north forest-R06 :south forest-R08
                          :east forest-S07))))

(deflocation forest-S07 (forest)
  (:exits ((forest-portal :west forest-R07 :north forest-S06 :south forest-S08
                          :east forest-T07))))

(deflocation forest-T07 (forest)
  (:exits ((forest-portal :west forest-S07 :north forest-T06))))

(deflocation forest-A08 (forest)
  (:exits ((forest-portal :north forest-A07 :south forest-A09 :east forest-B08))))

(deflocation forest-B08 (forest)
  (:exits ((forest-portal :west forest-A08 :north forest-B07 :south forest-B09
                          :east forest-C08))))

(deflocation forest-C08 (forest)
  (:exits ((forest-portal :west forest-B08 :north forest-C07 :south forest-C09
                          :east forest-D08))))

(deflocation forest-D08 (forest)
  (:exits ((forest-portal :west forest-C08 :north forest-D07 :south forest-D09
                          :east forest-E08))))

(deflocation forest-E08 (forest)
  (:exits ((forest-portal :west forest-D08 :north forest-E07 :south forest-E09
                          :east forest-F08))))

(deflocation forest-F08 (forest)
  (:exits ((forest-portal :west forest-E08 :north forest-F07 :south forest-F09
                          :east forest-G08))))

(deflocation forest-G08 (forest)
  (:exits ((forest-portal :west forest-F08 :north forest-G07 :south forest-G09
                          :east forest-H08))))

(deflocation forest-H08 (forest)
  (:exits ((forest-portal :west forest-G08 :north forest-H07 :south forest-H09
                          :east forest-I08))))

(deflocation forest-I08 (forest)
  (:exits ((forest-portal :west forest-H08 :north forest-I07 :south forest-I09
                          :east forest-J08))))

(deflocation forest-J08 (forest)
  (:exits ((forest-portal :west forest-I08 :north forest-J07 :south forest-J09
                          :east forest-K08))))

(deflocation forest-K08 (forest)
  (:exits ((forest-portal :west forest-J08 :north forest-K07 :south forest-K09
                          :east forest-L08))))

(deflocation forest-L08 (forest)
  (:exits ((forest-portal :west forest-K08 :north forest-L07 :south forest-L09
                          :east forest-M08))))

(deflocation forest-M08 (forest)
  (:exits ((forest-portal :west forest-L08 :north forest-M07 :south forest-M09
                          :east road-N08))))

(deflocation forest-O08 (forest)
  (:exits ((forest-portal :west road-N08 :north forest-O07 :south forest-O09
                          :east forest-stream-P08))))

(deflocation forest-Q08 (forest)
  (:exits ((forest-portal :west forest-stream-P08 :north forest-Q07
                          :south forest-Q09 :east forest-R08))))

(deflocation forest-R08 (forest)
  (:exits ((forest-portal :west forest-Q08 :north forest-R07 :south forest-R09
                          :east forest-S08))))

(deflocation forest-S08 (forest)
  (:exits ((forest-portal :west forest-R08 :north forest-S07))))

(deflocation forest-A09 (forest)
  (:exits ((forest-portal :north forest-A08 :east forest-B09))))

(deflocation forest-B09 (forest)
  (:exits ((forest-portal :west forest-A09 :north forest-B08 :east forest-C09))))

(deflocation forest-C09 (forest)
  (:exits ((forest-portal :west forest-B09 :north forest-C08 :east forest-D09))))

(deflocation forest-D09 (forest)
  (:exits ((forest-portal :west forest-C09 :north forest-D08 :south forest-D10
                          :east forest-E09))))

(deflocation forest-E09 (forest)
  (:exits ((forest-portal :west forest-D09 :north forest-E08 :south forest-E10
                          :east forest-F09))))

(deflocation forest-F09 (forest)
  (:exits ((forest-portal :west forest-E09 :north forest-F08 :south forest-F10
                          :east forest-G09))))

(deflocation forest-G09 (forest)
  (:exits ((forest-portal :west forest-F09 :north forest-G08 :south forest-G10
                          :east forest-H09))))

(deflocation forest-H09 (forest)
  (:exits ((forest-portal :west forest-G09 :north forest-H08 :south forest-H10
                          :east forest-I09))))

(deflocation forest-I09 (forest)
  (:exits ((forest-portal :west forest-H09 :north forest-I08 :south forest-I10
                          :east forest-J09))))

(deflocation forest-J09 (forest)
  (:exits ((forest-portal :west forest-I09 :north forest-J08 :south forest-J10
                          :east forest-K09))))

(deflocation forest-K09 (forest)
  (:exits ((forest-portal :west forest-J09 :north forest-K08 :south forest-K10
                          :east forest-L09))))

(deflocation forest-L09 (forest)
  (:exits ((forest-portal :west forest-K09 :north forest-L08 :south forest-L10
                          :east forest-M09))))

(deflocation forest-M09 (forest)
  (:exits ((forest-portal :west forest-L09 :north forest-M08 :south forest-M10
                          :east road-N09))))

(deflocation forest-O09 (forest)
  (:exits ((forest-portal :west road-N09 :north forest-O08 :south forest-O10
                          :east forest-stream-P09))))

(deflocation forest-Q09 (forest)
  (:exits ((forest-portal :west forest-stream-P09 :north forest-Q08
                          :south forest-Q10 :east forest-R09))))

(deflocation forest-R09 (forest)
  (:exits ((forest-portal :west forest-Q09 :north forest-R08 :south forest-R10))))

(deflocation forest-B10 (forest)
  (:exits ((forest-portal :southwest forest-A11 :east forest-C10))))

(deflocation forest-C10 (forest)
  (:exits ((forest-portal :west forest-B10 :east forest-D10))))

(deflocation forest-D10 (forest)
  (:exits ((forest-portal :west forest-C10 :north forest-D09 :east forest-E10))))

(deflocation forest-E10 (forest)
  (:exits ((forest-portal :west forest-D10 :north forest-E09 :south forest-E11
                          :east forest-F10))))

(deflocation forest-F10 (forest)
  (:exits ((forest-portal :west forest-E10 :north forest-F09 :south forest-F11
                          :east forest-G10))))

(deflocation forest-G10 (forest)
  (:exits ((forest-portal :west forest-F10 :north forest-G09 :south forest-G11
                          :east forest-H10))))

(deflocation forest-H10 (forest)
  (:exits ((forest-portal :west forest-G10 :north forest-H09 :south forest-H11
                          :east forest-I10))))

(deflocation forest-I10 (forest)
  (:exits ((forest-portal :west forest-H10 :north forest-I09 :south forest-I11
                          :east forest-J10))))

(deflocation forest-J10 (forest)
  (:exits ((forest-portal :west forest-I10 :north forest-J09 :south forest-J11
                          :east forest-K10))))

(deflocation forest-K10 (forest)
  (:exits ((forest-portal :west forest-J10 :north forest-K09 :south forest-K11
                          :east forest-L10))))

(deflocation forest-L10 (forest)
  (:exits ((forest-portal :west forest-K10 :north forest-L09 :south forest-L11
                          :east forest-M10))))

(deflocation forest-M10 (forest)
  (:exits ((forest-portal :west forest-L10 :north forest-M09 :south forest-M11
                          :east road-N10))))

(deflocation forest-O10 (forest)
  (:exits ((forest-portal :west road-N10 :north forest-O09 :south forest-O11
                          :east forest-stream-P10))))

(deflocation forest-Q10 (forest)
  (:exits ((forest-portal :west forest-stream-P10 :north forest-Q09
                          :south forest-Q11 :east forest-R10))))

(deflocation forest-R10 (forest)
  (:exits ((forest-portal :west forest-Q10 :north forest-R09 :south forest-R11
                          :east forest-S10))))

(deflocation forest-S10 (forest)
  (:exits ((forest-portal :west forest-R10 :south forest-S11))))

(deflocation forest-A11 (forest)
  (:exits ((forest-portal :northeast forest-B10))))

(deflocation forest-E11 (forest)
  (:exits ((forest-portal :north forest-E10 :south forest-E12 :east forest-F11))))

(deflocation forest-F11 (forest)
  (:exits ((forest-portal :west forest-E11 :north forest-F10 :south forest-F12
                          :east forest-G11))))

(deflocation forest-G11 (forest)
  (:exits ((forest-portal :west forest-F11 :north forest-G10 :south forest-G12
                          :east forest-H11))))

(deflocation forest-H11 (forest)
  (:exits ((forest-portal :west forest-G11 :north forest-H10 :south forest-H12
                          :east forest-I11))))

(deflocation forest-I11 (forest)
  (:exits ((forest-portal :west forest-H11 :north forest-I10 :south forest-I12
                          :east forest-J11))))

(deflocation forest-J11 (forest)
  (:exits ((forest-portal :west forest-I11 :north forest-J10 :south forest-J12
                          :east forest-K11))))

(deflocation forest-K11 (forest)
  (:exits ((forest-portal :west forest-J11 :north forest-K10 :south road-K12
                          :east forest-L11))))

(deflocation forest-L11 (forest)
  (:exits ((forest-portal :west forest-K11 :north forest-L10 :south road-L12
                          :east forest-M11))))

(deflocation forest-M11 (forest)
  (:exits ((forest-portal :west forest-L11 :north forest-M10 :south road-M12
                          :east road-N11))))

(deflocation forest-O11 (forest)
  (:exits ((forest-portal :west road-N11 :north forest-O10 :south forest-O12
                          :east forest-stream-P11))))

(deflocation forest-Q11 (forest)
  (:exits ((forest-portal :west forest-stream-P11 :north forest-Q10
                          :south forest-Q12 :east forest-R11))))

(deflocation forest-R11 (forest)
  (:exits ((forest-portal :west forest-Q11 :north forest-R10 :south forest-R12
                          :east forest-S11))))

(deflocation forest-S11 (forest)
  (:exits ((forest-portal :west forest-R11 :north forest-S10 :south forest-S12
                          :east forest-T11))))

(deflocation forest-T11 (forest)
  (:exits ((forest-portal :west forest-S11 :south forest-T12))))

(deflocation forest-C12 (forest)
  (:exits ((forest-portal :south forest-C13 :east forest-D12))))

(deflocation forest-D12 (forest)
  (:exits ((forest-portal :west forest-C12 :south forest-D13 :east forest-E12))))

(deflocation forest-E12 (forest)
  (:exits ((forest-portal :west forest-D12 :north forest-E11 :south forest-E13
                          :east forest-F12))))

(deflocation forest-F12 (forest)
  (:exits ((forest-portal :west forest-E12 :north forest-F11 :south forest-F13
                          :east forest-G12))))

(deflocation forest-G12 (forest)
  (:exits ((forest-portal :west forest-F12 :north forest-G11 :south forest-G13
                          :east forest-H12))))

(deflocation forest-H12 (forest)
  (:exits ((forest-portal :west forest-G12 :north forest-H11 :south forest-H13
                          :east forest-I12))))

(deflocation forest-I12 (forest)
  (:exits ((forest-portal :west forest-H12 :north forest-I11 :south forest-I13
                          :east forest-J12))))

(deflocation forest-J12 (forest)
  (:exits ((forest-portal :west forest-I12 :north forest-J11 :south forest-J13
                          :east road-K12))))

(deflocation forest-O12 (forest)
  (:exits ((forest-portal :west road-N12 :north forest-O11 :south forest-O13
                          :east forest-stream-P12))))

(deflocation forest-Q12 (forest)
  (:exits ((forest-portal :north forest-Q11 :east forest-R12))))

(deflocation forest-R12 (forest)
  (:exits ((forest-portal :west forest-Q12 :north forest-R11
                          :south forest-stream-R13 :east forest-S12))))

(deflocation forest-S12 (forest)
  (:exits ((forest-portal :west forest-R12 :north forest-S11 :south forest-S13
                          :east forest-T12))))

(deflocation forest-T12 (forest)
  (:exits ((forest-portal :west forest-S12 :north forest-T11 :south forest-T13))))

(deflocation forest-A13 (forest)
  (:exits ((forest-portal :south forest-A14 :east forest-B13))))

(deflocation forest-B13 (forest)
  (:exits ((forest-portal :west forest-A13 :south forest-B14 :east forest-C13))))

(deflocation forest-C13 (forest)
  (:exits ((forest-portal :west forest-B13 :north forest-C12 :south forest-C14
                          :east forest-D13))))

(deflocation forest-D13 (forest)
  (:exits ((forest-portal :west forest-C13 :north forest-D12 :south forest-D14
                          :east forest-E13))))

(deflocation forest-E13 (forest)
  (:exits ((forest-portal :west forest-D13 :north forest-E12 :south forest-E14
                          :east forest-F13))))

(deflocation forest-F13 (forest)
  (:exits ((forest-portal :west forest-E13 :north forest-F12 :south forest-F14
                          :east forest-G13))))

(deflocation forest-G13 (forest)
  (:exits ((forest-portal :west forest-F13 :north forest-G12 :south forest-G14
                          :east forest-H13))))

(deflocation forest-H13 (forest)
  (:exits ((forest-portal :west forest-G13 :north forest-H12 :south forest-H14
                          :east forest-I13))))

(deflocation forest-I13 (forest)
  (:exits ((forest-portal :west forest-H13 :north forest-I12 :south road-I14
                          :east forest-J13))))

(deflocation forest-J13 (forest)
  (:exits ((forest-portal :west forest-I13 :north forest-J12 :south road-J14
                          :east road-K13))))

(deflocation forest-L13 (forest)
  (:exits ((forest-portal :west road-K13 :north road-L12 :south forest-L14
                          :east forest-M13))))

(deflocation forest-M13 (forest)
  (:exits ((forest-portal :west forest-L13 :north road-M12 :south forest-M14
                          :east ranger-camp-N13))))

(deflocation forest-O13 (forest)
  (:exits ((forest-portal :west ranger-camp-N13 :north forest-O12 :south forest-O14
                          :east forest-stream-P13))))

(deflocation forest-S13 (forest)
  (:exits ((forest-portal :west forest-stream-R13 :north forest-S12
                          :south forest-S14 :east forest-T13))))

(deflocation forest-T13 (forest)
  (:exits ((forest-portal :west forest-S13 :north forest-T12 :south forest-T14))))

(deflocation forest-A14 (forest)
  (:exits ((forest-portal :north forest-A13 :south forest-A15 :east forest-B14))))

(deflocation forest-B14 (forest)
  (:exits ((forest-portal :west forest-A14 :north forest-B13 :south forest-B15
                          :east forest-C14))))

(deflocation forest-C14 (forest)
  (:exits ((forest-portal :west forest-B14 :north forest-C13 :south forest-C15
                          :east forest-D14))))

(deflocation forest-D14 (forest)
  (:exits ((forest-portal :west forest-C14 :north forest-D13 :south forest-D15
                          :east forest-E14))))

(deflocation forest-E14 (forest)
  (:exits ((forest-portal :west forest-D14 :north forest-E13 :south forest-E15
                          :east forest-F14))))

(deflocation forest-F14 (forest)
  (:exits ((forest-portal :west forest-E14 :north forest-F13 :south forest-F15
                          :east forest-G14))))

(deflocation forest-G14 (forest)
  (:exits ((forest-portal :west forest-F14 :north forest-G13 :south forest-G15
                          :east forest-H14))))

(deflocation forest-H14 (forest)
  (:exits ((forest-portal :west forest-G14 :north forest-H13 :south forest-H15
                          :east road-I14))))

(deflocation forest-L14 (forest)
  (:exits ((forest-portal :west road-K14 :north forest-L13 :south forest-L15
                          :east forest-M14))))

(deflocation forest-M14 (forest)
  (:exits ((forest-portal :west forest-L14 :north forest-M13 :south forest-M15
                          :east forest-N14))))

(deflocation forest-N14 (forest)
  (:exits ((forest-portal :west forest-M14 :north ranger-camp-N13 :south forest-N15
                          :east forest-O14))))

(deflocation forest-O14 (forest)
  (:exits ((forest-portal :west forest-N14 :north forest-O13 :south forest-O15
                          :east forest-P14))))

(deflocation forest-P14 (forest)
  (:exits ((forest-portal :west forest-O14 :north forest-stream-P13 :east forest-Q14))))

(deflocation forest-Q14 (forest)
  (:exits ((forest-portal :west forest-P14 :north forest-stream-Q13))))

(deflocation forest-S14 (forest)
  (:exits ((forest-portal :north forest-S13 :east forest-T14))))

(deflocation forest-T14 (forest)
  (:exits ((forest-portal :west forest-S14 :north forest-T13))))

(deflocation forest-A15 (forest)
  (:exits ((forest-portal :north forest-A14 :east forest-B15))))

(deflocation forest-B15 (forest)
  (:exits ((forest-portal :west forest-A15 :north forest-B14 :south forest-B16
                          :east forest-C15))))

(deflocation forest-C15 (forest)
  (:exits ((forest-portal :west forest-B15 :north forest-C14 :south forest-C16
                          :east forest-D15))))

(deflocation forest-D15 (forest)
  (:exits ((forest-portal :west forest-C15 :north forest-D14 :south forest-D16
                          :east forest-E15))))

(deflocation forest-E15 (forest)
  (:exits ((forest-portal :west forest-D15 :north forest-E14 :south forest-E16
                          :east forest-F15))))

(deflocation forest-F15 (forest)
  (:exits ((forest-portal :west forest-E15 :north forest-F14 :south forest-F16
                          :east forest-G15))))

(deflocation forest-G15 (forest)
  (:exits ((forest-portal :west forest-F15 :north forest-G14 :south forest-G16
                          :east forest-H15))))

(deflocation forest-H15 (forest)
  (:exits ((forest-portal :west forest-G15 :north forest-H14 :south forest-H16
                          :east road-I15))))

(deflocation forest-J15 (forest)
  (:exits ((forest-portal :west road-I15 :north road-J14 :south forest-J16
                          :east forest-K15))))

(deflocation forest-K15 (forest)
  (:exits ((forest-portal :west forest-J15 :north road-K14 :south forest-K16
                          :east forest-L15))))

(deflocation forest-L15 (forest)
  (:exits ((forest-portal :west forest-K15 :north forest-L14 :south forest-L16
                          :east forest-M15))))

(deflocation forest-M15 (forest)
  (:exits ((forest-portal :west forest-L15 :north forest-M14 :south forest-M16
                          :east forest-N15))))

(deflocation forest-N15 (forest)
  (:exits ((forest-portal :west forest-M15 :north forest-N14 :south forest-N16
                          :east forest-O15))))

(deflocation forest-O15 (forest)
  (:exits ((forest-portal :west forest-N15 :north forest-O14 :south forest-O16))))

(deflocation forest-B16 (forest)
  (:exits ((forest-portal :north forest-B15 :east forest-C16))))

(deflocation forest-C16 (forest)
  (:exits ((forest-portal :west forest-B16 :north forest-C15 :south forest-C17
                          :east forest-D16))))

(deflocation forest-D16 (forest)
  (:exits ((forest-portal :west forest-C16 :north forest-D15 :south forest-D17
                          :east forest-E16))))

(deflocation forest-E16 (forest)
  (:exits ((forest-portal :west forest-D16 :north forest-E15 :south forest-E17
                          :east forest-F16))))

(deflocation forest-F16 (forest)
  (:exits ((forest-portal :west forest-E16 :north forest-F15 :south forest-F17
                          :east forest-G16))))

(deflocation forest-G16 (forest)
  (:exits ((forest-portal :west forest-F16 :north forest-G15 :south forest-G17
                          :east forest-H16))))

(deflocation forest-H16 (forest)
  (:exits ((forest-portal :west forest-G16 :north forest-H15 :south forest-H17
                          :east road-I16))))

(deflocation forest-J16 (forest)
  (:exits ((forest-portal :west road-I16 :north forest-J15 :south forest-J17
                          :east forest-K16))))

(deflocation forest-K16 (forest)
  (:exits ((forest-portal :west forest-J16 :north forest-K15 :south forest-K17
                          :east forest-L16))))

(deflocation forest-L16 (forest)
  (:exits ((forest-portal :west forest-K16 :north forest-L15 :south forest-L17
                          :east forest-M16))))

(deflocation forest-M16 (forest)
  (:exits ((forest-portal :west forest-L16 :north forest-M15 :south forest-M17
                          :east forest-N16))))

(deflocation forest-N16 (forest)
  (:exits ((forest-portal :west forest-M16 :north forest-N15 :east forest-O16))))

(deflocation forest-O16 (forest)
  (:exits ((forest-portal :west forest-N16 :north forest-O15 :southeast forest-P17))))

(deflocation forest-C17 (forest)
  (:exits ((forest-portal :north forest-C16 :east forest-D17))))

(deflocation forest-D17 (forest)
  (:exits ((forest-portal :west forest-C17 :north forest-D16 :east forest-E17))))

(deflocation forest-E17 (forest)
  (:exits ((forest-portal :west forest-D17 :north forest-E16 :east forest-F17))))

(deflocation forest-F17 (forest)
  (:exits ((forest-portal :west forest-E17 :north forest-F16 :east forest-G17))))

(deflocation forest-G17 (forest)
  (:exits ((forest-portal :west forest-F17 :north forest-G16 :east forest-H17))))

(deflocation forest-H17 (forest)
  (:exits ((forest-portal :west forest-G17 :north forest-H16 :east road-I17))))

(deflocation forest-J17 (forest)
  (:exits ((forest-portal :west road-I17 :north forest-J16 :east forest-K17))))

(deflocation forest-K17 (forest)
  (:exits ((forest-portal :west forest-J17 :north forest-K16 :east forest-L17))))

(deflocation forest-L17 (forest)
  (:exits ((forest-portal :west forest-K17 :north forest-L16 :east forest-M17))))

(deflocation forest-M17 (forest)
  (:exits ((forest-portal :west forest-L17 :north forest-M16))))

(deflocation forest-P17 (forest)
  (:exits ((forest-portal :northwest forest-O16 :east forest-Q17))))

(deflocation forest-Q17 (forest)
  (:exits ((forest-portal :west forest-P17))))

;;; behind-the-waterfall

(defentity behind-waterfall-portal ()
  (:brief "a waterfall"
   :pose "obscures the entrance to this small cave."))

(defentity bespectacled-frog ()
  (:brief "a bespectacled frog"
   :pose "sits on a mossy rock with a frown on its face. The frog, not the rock.
     Rocks don't have faces."
   :description "The frog is both abnormally large and apparently quite
     near-sighted.")

  (:after-enter-location ((actor avatar) location entry)
    (with-delay (1)
      (show actor "The frog shakes its head as your approach.")))

  (:when-talk (actor self (topic nil))
    (tell self actor "What is it with explorers like you, always looking for
      secret spaces behind waterfalls? I just wanted to be alone and contemplate
      life. Is that too much to ask?

      Now that you've disturbed me, you may as well take advantage of my
      expertise. I know quite a bit about the happenings in the local area. What
      do you want to know about?")
    (maybe-show-tutorial actor 'talk-topics "You can discuss something specific
      with the frog by specifying a topic with the `talk` command. For example,
      `talk to frog about flies`."))

  (:when-talk (actor self (topic "flies" "fly"))
    (tell self actor "Ahh, flies ... I can never get enough! The sheer ecstasy
      of catching them with my tongue, their subtle texture ... urglglgl ...

      Ahem. That is not a topic I need to discuss further. Perhaps ask about
      something else?"))

  (:when-talk (actor self (topic "kobold" "copper"))
    (tell self actor "I have seen kobolds lurking around the northwestern corner
      of the forest. They have a mine in that area, said to be rich with copper
      deposits. If you are a miner, you might find it worth visiting."))

  (:when-talk (actor self topic)
    (tell self actor "I'm afraid I don't know anything about that.")))

(deflocation behind-the-waterfall ()
  (:name "Behind the Waterfall"
   :domain :underground
   :contents (bespectacled-frog)
   :exits ((behind-waterfall-portal :south forest-stream-P00))))

;;; forest-stream

(defentity forest-stream (location)
  (:name "Swift Stream"
   :description "This burbling, turbulent stream of cold water runs north to
     south through the forest."
   :domain :outdoor
   :surface :shallow-water))

(defentity waterfall-portal ()
  (:brief "a waterfall"
   :pose "cascades down the rocky cliff."
   :hidden t))

(deflocation forest-stream-P00 (forest-stream)
  (:name "Shallow Pool"
   :description "A shallow pool has formed at the base of a small waterfall to
     the north. Spray from the waterfall hangs in the air. Thick moss covers the
     rocks that surround the pool."
   :exits ((forest-portal :west forest-O00 :east forest-Q00)
           (stream-portal :south forest-stream-P01)
           (waterfall-portal :north behind-the-waterfall))))

(deflocation forest-stream-P01 (forest-stream)
  (:exits ((forest-portal :west forest-O01 :east forest-Q01)
           (stream-portal :north forest-stream-P00 :south forest-stream-P02))))

(deflocation forest-stream-P02 (forest-stream)
  (:exits ((forest-portal :west forest-O02 :east forest-Q02)
           (stream-portal :north forest-stream-P01 :south forest-stream-P03))))

(deflocation forest-stream-P03 (forest-stream)
  (:exits ((forest-portal :west forest-O03 :east forest-Q03)
           (stream-portal :north forest-stream-P02 :south forest-stream-P04))))

(deflocation forest-stream-P04 (forest-stream)
  (:exits ((forest-portal :west forest-O04 :east forest-Q04)
           (stream-portal :north forest-stream-P03 :south under-the-bridge))))

(deflocation under-the-bridge (forest-stream)
  (:name "Under the Bridge"
   :description "The stream passes beneath an arched bridge of white stone.
     Someone has scratched a few words into the underside of the bridge; it is
     nearly illegible, but you think it says \"Anthony was here.\""
   :z-offset -1
   :exits ((stream-portal :north forest-stream-P04 :south forest-stream-P06))))

(deflocation forest-stream-P06 (forest-stream)
  (:exits ((forest-portal :west forest-O06 :east forest-Q06)
           (stream-portal :south forest-stream-P07 :north under-the-bridge))))

(deflocation forest-stream-P07 (forest-stream)
  (:exits ((forest-portal :west forest-O07 :east forest-Q07)
           (stream-portal :north forest-stream-P06 :south forest-stream-P08))))

(deflocation forest-stream-P08 (forest-stream)
  (:exits ((forest-portal :west forest-O08 :east forest-Q08)
           (stream-portal :north forest-stream-P07 :south forest-stream-P09))))

(deflocation forest-stream-P09 (forest-stream)
  (:exits ((forest-portal :west forest-O09 :east forest-Q09)
           (stream-portal :north forest-stream-P08 :south forest-stream-P10))))

(deflocation forest-stream-P10 (forest-stream)
  (:exits ((forest-portal :west forest-O10 :east forest-Q10)
           (stream-portal :north forest-stream-P09 :south forest-stream-P11))))

(deflocation forest-stream-P11 (forest-stream)
  (:exits ((forest-portal :west forest-O11 :east forest-Q11)
           (stream-portal :north forest-stream-P10 :south forest-stream-P12))))

(deflocation forest-stream-P12 (forest-stream)
  (:exits ((forest-portal :west forest-O12)
           (stream-portal :north forest-stream-P11 :south forest-stream-P13))))

(deflocation forest-stream-P13 (forest-stream)
  (:exits ((forest-portal :west forest-O13 :south forest-P14)
           (stream-portal :north forest-stream-P12 :east forest-stream-Q13))))

(deflocation forest-stream-Q13 (forest-stream)
  (:exits ((stream-portal :west forest-stream-P13 :east forest-stream-R13)
           (forest-portal :south forest-Q14))))

(deflocation forest-stream-R13 (forest-stream)
  (:exits ((stream-portal :west forest-stream-Q13 :south canyon-R14)
           (forest-portal :north forest-R12 :east forest-S13))))

;;; cabin

(defentity cabin-exterior ()
  (:brief "a tiny cabin"
   :pose "stands in the middle of the clearing."
   :description "The cabin's exterior is decorated in bright colors. Smoke rises from a
    stone chimney. A gaily painted door leads into the cabin."
   :implicit-neighbor t))

(defentity cabin-outside-door (entry-doorway)
  (:brief "a gaily painted door"
   :exit-verb "heads into the cabin."
   :entry-verb "comes from inside the cabin."))

(defentity cabin-inside-door (exit-doorway)
  (:brief "a gaily painted door"
   :exit-verb "leaves the cabin."
   :entry-verb "enters the cabin."))

(defentity old-sword ()
  (:brief "a single-edged sword"
   :pose "hangs above the hearth."
   :icon swords-10
   :description "Although clearly old and in need of sharpening, the sword
     appears to be of excellent craftsmanship. Its long hilt is wrapped with
     dark cord and its small circular guard is ornately carved.

     A slip of parchment is tacked to the wall below the sword. It reads:

     > This blade is given to Knight-Captain Zara Alenys in recognition of her
     heroism at the Battle of Three Rivers. May it help her succeed in her
     quest."))

(defquest find-spiderwebs
  (:name "Silk for Sewing"
   :summary "Collect large spiderwebs for the hermit in Silverwood."
   :level 2)

  (:active
    :summary "Collect five large spiderwebs."
    :initial-state 0)

  (:done
    :summary "Return to the hermit."))

(defentity large-spiderweb (item)
  (:brief "a large spiderweb"
   :pose "is visible in the branches of a nearby tree."
   :description "The strands of this web are especially strong and thick."
   :icon spider-web
   :alts ("large web")
   :stackable 5
   :quest find-spiderwebs)

  (:allow-take ((actor &quest find-spiderwebs :active) self container))

  (:allow-take (actor self container)
    (show actor "You cannot take that right now.")
    (disallow-action))

  (:after-take ((actor &quest find-spiderwebs :active) self container)
    (advance-quest self actor 'find-spiderwebs 1/5)))

(limit-spawn-quantity 'large-spiderweb 15)

(defentity hermit (humanoid)
  (:brief "an old hermit"
   :pose "whistles as she mends a shirt."
   :description "The hermit is an elderly woman, her skin deeply wrinkled from
     many years of wind and sun. She is stooped and walks only with great
     effort, but her eyes and mind are as sharp as ever. An old scar runs down
     the left side of her face."
   :offers-quests (find-spiderwebs))

  (:when-talk ((actor &quest find-spiderwebs :available) self topic)
    (tell self actor "Ah, a visitor. Few people come to my little house these
      days, what with the spiders and worse lurking in the woods. Speaking of
      spiders, I could use your help if you are willing.")
    (offer-quest self 'find-spiderwebs actor))

  (:after-accept-quest (actor (quest &quest find-spiderwebs) self)
    (tell self actor "As you can see I like to sew; it helps me pass the time
      and keeps my nieces and nephews from running around bare naked! There's
      just one problem: I need spider silk to make thread. Perhaps you can bring
      me a few of the largest webs? You can find them all over the forest, but
      watch out for the webspinners! They can be dangerous."))

  (:when-talk ((actor &quest find-spiderwebs :active) self topic)
    (tell self actor "Having trouble finding webs? Just look around the woods,
      you'll see them up in the branches."))

  (:when-talk ((actor &quest find-spiderwebs :done) self topic)
    (tell self actor "Thank you! You can rest assured that I will put these to
      good use.")
    (advance-quest self actor 'find-spiderwebs)))

(deflocation cabin ()
  (:name "Cabin in the Woods"
   :description "This one-room cabin is tiny but impeccably maintained."
   :domain :indoor
   :surface :wood
   :contents (old-sword hermit)
   :exits ((cabin-inside-door :out clearing-E05))))

;;; clearing

(defentity clearing (location)
  (:name "Small Clearing"
   :description "This grassy area is surrounded by the forest."
   :domain :outdoor
   :surface :grass))

(deflocation clearing-D04 (clearing)
  (:exits ((forest-portal :west forest-C04 :north forest-D03)
           (clearing-portal :south clearing-D05 :east clearing-E04))))

(deflocation clearing-E04 (clearing)
  (:exits ((clearing-portal :west clearing-D04 :south clearing-E05 :east clearing-F04)
           (forest-portal :north forest-E03))))

(deflocation clearing-F04 (clearing)
  (:exits ((clearing-portal :west clearing-E04 :south clearing-F05)
           (forest-portal :north forest-F03 :east forest-G04))))

(deflocation clearing-D05 (clearing)
  (:exits ((forest-portal :west forest-C05)
           (clearing-portal :north clearing-D04 :south clearing-D06 :east clearing-E05))))

(deflocation clearing-E05 (clearing)
  (:contents (cabin-exterior)
   :icon cabin
   :exits ((clearing-portal :west clearing-D05 :north clearing-E04
                            :south clearing-E06 :east clearing-F05)
           (cabin-outside-door :in cabin))))

(deflocation clearing-F05 (clearing)
  (:exits ((clearing-portal :west clearing-E05 :north clearing-F04 :south clearing-F06)
           (forest-portal :east forest-G05))))

(deflocation clearing-D06 (clearing)
  (:exits ((forest-portal :west forest-C06 :south forest-D07)
           (clearing-portal :north clearing-D05 :east clearing-E06))))

(deflocation clearing-E06 (clearing)
  (:exits ((clearing-portal :west clearing-D06 :north clearing-E05 :east clearing-F06)
           (forest-portal :south forest-E07))))

(deflocation clearing-F06 (clearing)
  (:exits ((clearing-portal :west clearing-E06 :north clearing-F05)
           (forest-portal :south forest-F07 :east forest-G06))))

;;; road

(defentity road (location)
  (:name "Forest Road"
   :domain :outdoor
   :surface :dirt))

(deflocation road-N05 (road)
  (:exits ((forest-portal :west forest-M05 :north forest-N04)
           (road-portal :south road-N06 :east road-O05))))

(deflocation road-O05 (road)
  (:exits ((road-portal :west road-N05 :east bridge-P05)
           (forest-portal :north forest-O04 :south forest-O06))))

(deflocation road-Q05 (road)
  (:exits ((road-portal :west bridge-P05 :east road-R05)
           (forest-portal :north forest-Q04 :south forest-Q06))))

(deflocation road-R05 (road)
  (:exits ((road-portal :west road-Q05 :east road-S05)
           (forest-portal :north forest-R04 :south forest-R06))))

(deflocation road-S05 (road)
  (:exits ((road-portal :west road-R05 :east road-T05)
           (forest-portal :north forest-S04 :south forest-S06))))

(deflocation road-T05 (road)
  (:exits ((road-portal :west road-S05 :east jade.arwyck::forest-gate)
           (forest-portal :north forest-T04 :south forest-T06))))

(deflocation road-N06 (road)
  (:exits ((forest-portal :west forest-M06 :east forest-O06)
           (road-portal :north road-N05 :south road-N07))))

(deflocation road-N07 (road)
  (:exits ((forest-portal :west forest-M07 :east forest-O07)
           (road-portal :north road-N06 :south road-N08))))

(deflocation road-N08 (road)
  (:exits ((forest-portal :west forest-M08 :east forest-O08)
           (road-portal :north road-N07 :south road-N09))))

(deflocation road-N09 (road)
  (:exits ((forest-portal :west forest-M09 :east forest-O09)
           (road-portal :north road-N08 :south road-N10))))

(deflocation road-N10 (road)
  (:exits ((forest-portal :west forest-M10 :east forest-O10)
           (road-portal :north road-N09 :south road-N11))))

(deflocation road-N11 (road)
  (:exits ((forest-portal :west forest-M11 :east forest-O11)
           (road-portal :north road-N10 :south road-N12))))

(deflocation road-K12 (road)
  (:exits ((forest-portal :west forest-J12 :north forest-K11)
           (road-portal :south road-K13 :east road-L12))))

(deflocation road-L12 (road)
  (:exits ((road-portal :west road-K12 :east road-M12)
           (forest-portal :north forest-L11 :south forest-L13))))

(deflocation road-M12 (road)
  (:exits ((road-portal :west road-L12 :east road-N12)
           (forest-portal :north forest-M11 :south forest-M13))))

(deflocation road-N12 (road)
  (:exits ((road-portal :west road-M12 :north road-N11)
           (narrow-track :south ranger-camp-N13)
           (forest-portal :east forest-O12))))

(deflocation road-K13 (road)
  (:exits ((forest-portal :west forest-J13 :east forest-L13)
           (road-portal :north road-K12 :south road-K14))))

(deflocation road-I14 (road)
  (:exits ((forest-portal :west forest-H14 :north forest-I13)
           (road-portal :south road-I15 :east road-J14))))

(deflocation road-J14 (road)
  (:exits ((road-portal :west road-I14 :east road-K14)
           (forest-portal :north forest-J13 :south forest-J15))))

(deflocation road-K14 (road)
  (:exits ((road-portal :west road-J14 :north road-K13)
           (forest-portal :south forest-K15 :east forest-L14))))

(deflocation road-I15 (road)
  (:exits ((forest-portal :west forest-H15 :east forest-J15)
           (road-portal :north road-I14 :south road-I16))))

(deflocation road-I16 (road)
  (:exits ((forest-portal :west forest-H16 :east forest-J16)
           (road-portal :north road-I15 :south road-I17))))

(deflocation road-I17 (road)
  (:exits ((forest-portal :west forest-H17 :east forest-J17)
           (road-portal :north road-I16))))

;;; bridge

(defentity bridge (location)
  (:name "Stone Bridge"
   :description "You stand on an arched bridge of white stone that spans the
     gurgling stream below."
   :domain :outdoor
   :surface :stone))

(deflocation bridge-P05 (bridge)
  (:exits ((road-portal :west road-O05 :east road-Q05))))

;;; ranger-camp

(defentity ranger-camp (location)
  (:name "Rangers' Camp"
   :domain :outdoor
   :surface :grass))

(deflocation ranger-camp-N13 (ranger-camp)
  (:exits ((forest-portal :west forest-M13 :south forest-N14 :east forest-O13)
           (narrow-track :north road-N12))))

;;; canyon

(defentity canyon (location)
  (:name "Narrow Canyon"
   :description "A swirling stream of cold water runs north to south through a
     narrow, steep-walled canyon."
   :domain :outdoor
   :surface :shallow-water))

(deflocation canyon-R14 (canyon)
  (:exits ((stream-portal :north forest-stream-R13)
           (canyon-portal :south canyon-R15))))

(deflocation canyon-R15 (canyon)
  (:exits ((canyon-portal :north canyon-R14 :south canyon-R16))))

(deflocation canyon-R16 (canyon)
  (:exits ((canyon-portal :north canyon-R15 :south canyon-R17))))

(defentity crevice ()
  (:brief "a narrow crevice"
   :pose "leads into the cliff face."
   :entry-message "You carefully follow a narrow ledge alongside the turbulent
     water."))

(deflocation canyon-R17 (canyon)
  (:name "Canyon's End"
   :description "The narrow canyon comes to an abrupt end, enclosed on three sides by
     steep cliff walls. The waters of the stream froth and churn amongst numerous huge
     boulders before disappearing into a crevice in the south wall."
   :exits ((canyon-portal :north canyon-R16)
           (crevice :south nil)))) ; FIXME: jade.dripping-caverns::entrance
