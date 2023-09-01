(in-package :jade.copper-mine)

(defentity copper-mine ()
  (:name "Abandoned Copper Mine"
   :description "This mine once yielded plentiful copper ore, but was abandoned
     years ago due to the increasing dangers in the nearby woods. It has since been
     overrun by kobolds."
   :climate :underground
   :level-range (1 10)))

#|
    A B C D E F G H I J K L M N O P Q R S T U V W

00  . . . . m-m-m-m . . . . . . . . M . . . . . .
            | | | |                 |
01  . . . m-m-m-m-m-m . . M-M-M . . M . . V . . .
          | | | | | |     |    \    |     |
02  . . . m-m-m-p-m-m . . M . . M-M M . C-V . . .
          | | |           |       | |     |
03  . . . m-m-m . . . M-M-M . M-M-M-M . . V . . .
              |       |   |  /      |     |
04  . . . M-M-M-M . . M . M-M . . . M . . G . . .
         /      |     |   |         |     |
05  . M-M . . . M . . M . M . M-M-M-M . . M-M-M .
        |       |     |      /    |       |   |
06  . . M M-M-M-M-M-M-M-M-M-M . . M . . . M . M .
        |       |     |      \    |           |
07  . . M . . . M . . M . . . M-M-M-M-M . . . M .
         \     /      |               |      /
08  . . . M-M-M . . . M . . . . M-M-M-M-M-M-M . .
            |         |         |
09  . . . . M . . . . M . . . . M-M-M . . . . . .

|#

;;; kobolds

(defentity kobold (combatant)
  (:icon kobold))

(defentity kobold-pickaxe (one-handed-weapon)
  (:brief "a rusty pickaxe"
   :level 2
   :damage-type piercing))

(defentity kobold-miner (kobold)
  (:brief "a kobold miner"
   :description "The kobold wears a dented helmet topped with an unlit candle.
     It carries a rusty pickaxe."
   :level 2
   :attacks (kobold-pickaxe))

  (:after-enter-world ()
    (with-random-interval (120 240)
      (say self "You no take candle!"))))

(limit-spawn-quantity 'kobold-miner 15)

(defentity kobold-shortsword (one-handed-weapon)
  (:brief "a rusty shortsword"
   :level 3
   :damage-type slashing))

(defentity kobold-guard (kobold)
  (:brief "a kobold guard"
   :description "The kobold guard wears a brigandine made from the hide of some
     unknown creature. It carries a rusty shortsword."
   :level 3
   :attacks (kobold-shortsword)))

(limit-spawn-quantity 'kobold-guard 5)

;;; portal prototypes

(defentity mine-portal (continuing-portal)
  (:brief "the mine"))

(defentity hall-portal (continuing-portal)
  (:brief "the hall"))

(defentity cell-door ()
  (:brief "an iron door"))

(defentity cave-portal (continuing-portal)
  (:brief "the cave"))

(defentity tiny-passage ()
  (:brief "a tiny hole in the wall"
   :size +tiny+))

(defentity narrow-opening ()
  (:brief "a narrow opening"))

(defentity ladder ()
  (:brief "a rickety ladder"))

;;; mushroom-cave

(defentity mushroom-cave (location)
  (:name "Mushroom Cave"
   :domain :underground
   :surface :dirt))

(deflocation mushroom-cave-E00 (mushroom-cave)
  (:exits ((cave-portal :south mushroom-cave-E01 :east mushroom-cave-F00))))

(deflocation mushroom-cave-F00 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-E00 :south mushroom-cave-F01
                        :east mushroom-cave-G00))))

(deflocation mushroom-cave-G00 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-F00 :south mushroom-cave-G01
                        :east mushroom-cave-H00))))

(deflocation mushroom-cave-H00 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-G00 :south mushroom-cave-H01))))

(deflocation mushroom-cave-D01 (mushroom-cave)
  (:exits ((cave-portal :south mushroom-cave-D02 :east mushroom-cave-E01))))

(deflocation mushroom-cave-E01 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-D01 :north mushroom-cave-E00
                        :south mushroom-cave-E02 :east mushroom-cave-F01))))

(deflocation mushroom-cave-F01 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-E01 :north mushroom-cave-F00
                        :south mushroom-cave-F02 :east mushroom-cave-G01))))

(deflocation mushroom-cave-G01 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-F01 :north mushroom-cave-G00
                        :south murky-pool-G02 :east mushroom-cave-H01))))

(deflocation mushroom-cave-H01 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-G01 :north mushroom-cave-H00
                        :south mushroom-cave-H02 :east mushroom-cave-I01))))

(deflocation mushroom-cave-I01 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-H01 :south mushroom-cave-I02))))

(deflocation mushroom-cave-D02 (mushroom-cave)
  (:exits ((cave-portal :north mushroom-cave-D01 :south mushroom-cave-D03
                        :east mushroom-cave-E02))))

(deflocation mushroom-cave-E02 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-D02 :north mushroom-cave-E01
                        :south mushroom-cave-E03 :east mushroom-cave-F02))))

(deflocation mushroom-cave-F02 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-E02 :north mushroom-cave-F01
                        :south mushroom-cave-F03 :east murky-pool-G02))))

(deflocation mushroom-cave-H02 (mushroom-cave)
  (:exits ((cave-portal :west murky-pool-G02 :north mushroom-cave-H01
                        :east mushroom-cave-I02))))

(deflocation mushroom-cave-I02 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-H02 :north mushroom-cave-I01))))

(deflocation mushroom-cave-D03 (mushroom-cave)
  (:exits ((cave-portal :north mushroom-cave-D02 :east mushroom-cave-E03))))

(deflocation mushroom-cave-E03 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-D03 :north mushroom-cave-E02
                        :east mushroom-cave-F03))))

(deflocation mushroom-cave-F03 (mushroom-cave)
  (:exits ((cave-portal :west mushroom-cave-E03 :north mushroom-cave-F02)
           (tiny-passage :south mine-F04))))

;;; mine

(defentity mine (location)
  (:name "Mine"
   :description "This low, narrow tunnel is dimly lit by strange luminescent
     crystals embedded in the walls."
   :domain :underground
   :surface :stone)

  (:after-enter-world ()
    (with-random-interval (10 120)
      (whichever
       (spawn-unique-entity self 'kobold-miner)
       (spawn-unique-entity self 'kobold-guard)))))

(deflocation mine-Q00 (mine)
  (:exits ((mine-portal :south mine-Q01))))

(deflocation mine-L01 (mine)
  (:exits ((mine-portal :south mine-L02 :east mine-M01))))

(deflocation mine-M01 (mine)
  (:exits ((mine-portal :west mine-L01 :east mine-N01))))

(deflocation mine-N01 (mine)
  (:exits ((mine-portal :west mine-M01 :southeast mine-O02))))

(deflocation mine-Q01 (mine)
  (:exits ((mine-portal :north mine-Q00 :south mine-Q02))))

(deflocation mine-L02 (mine)
  (:exits ((mine-portal :north mine-L01 :south mine-L03-bottom))))

(deflocation mine-O02 (mine)
  (:exits ((mine-portal :northwest mine-N01 :east mine-P02))))

(deflocation mine-P02 (mine)
  (:exits ((mine-portal :west mine-O02 :south mine-P03))))

(deflocation mine-Q02 (mine)
  (:exits ((mine-portal :north mine-Q01 :south mine-Q03))))

(deflocation mine-J03 (mine)
  (:exits ((mine-portal :south mine-J04 :east mine-K03))))

(deflocation mine-K03 (mine)
  (:exits ((mine-portal :west mine-J03 :east mine-L03-top))))

(deflocation mine-L03-top (mine)
  (:exits ((mine-portal :west mine-K03) (ladder :down mine-L03-bottom))))

(deflocation mine-L03-bottom (mine)
  (:exits ((mine-portal :north mine-L02 :south mine-L04)
           (ladder :up mine-L03-top))))

(deflocation mine-N03 (mine)
  (:exits ((mine-portal :southwest mine-M04 :east mine-O03))))

(deflocation mine-O03 (mine)
  (:exits ((mine-portal :west mine-N03 :east mine-P03))))

(deflocation mine-P03 (mine)
  (:exits ((mine-portal :west mine-O03 :north mine-P02 :east mine-Q03))))

(deflocation mine-Q03 (mine)
  (:exits ((mine-portal :west mine-P03 :north mine-Q02 :south mine-Q04-top))))

(deflocation mine-D04 (mine)
  (:exits ((mine-portal :southwest mine-C05 :east mine-E04))))

(deflocation mine-E04 (mine)
  (:exits ((mine-portal :west mine-D04 :east mine-F04))))

(deflocation mine-F04 (mine)
  (:exits ((mine-portal :west mine-E04 :east mine-G04)
           (tiny-passage :north mushroom-cave-F03))))

(deflocation mine-G04 (mine)
  (:exits ((mine-portal :west mine-F04 :south mine-G05))))

(deflocation mine-J04 (mine)
  (:exits ((mine-portal :north mine-J03 :south mine-J05))))

(deflocation mine-L04 (mine)
  (:exits ((mine-portal :north mine-L03-bottom :south mine-L05 :east mine-M04))))

(deflocation mine-M04 (mine)
  (:exits ((mine-portal :west mine-L04 :northeast mine-N03))))

(deflocation mine-Q04-top (mine)
  (:exits ((mine-portal :north mine-Q03) (ladder :down mine-Q04-bottom))))

(deflocation mine-Q04-bottom (mine)
  (:exits ((mine-portal :south mine-Q05) (ladder :up mine-Q04-top))))

(deflocation mine-B05 (mine)
  (:exits ((mine-portal :east mine-C05))))

(deflocation mine-C05 (mine)
  (:exits ((mine-portal :west mine-B05 :south mine-C06 :northeast mine-D04))))

(deflocation mine-G05 (mine)
  (:exits ((mine-portal :north mine-G04 :south mine-G06))))

(deflocation mine-J05 (mine)
  (:exits ((mine-portal :north mine-J04 :south mine-J06-top))))

(deflocation mine-L05 (mine)
  (:exits ((mine-portal :north mine-L04))))

(deflocation mine-N05 (mine)
  (:exits ((mine-portal :southwest mine-M06-bottom :east mine-O05))))

(deflocation mine-O05 (mine)
  (:exits ((mine-portal :west mine-N05 :east mine-P05))))

(deflocation mine-P05 (mine)
  (:exits ((mine-portal :west mine-O05 :south mine-P06 :east mine-Q05))))

(deflocation mine-Q05 (mine)
  (:exits ((mine-portal :west mine-P05 :north mine-Q04-bottom))))

(deflocation mine-T05 (mine)
  (:exits ((mine-portal :north guard-station-T04 :south mine-T06 :east mine-U05))))

(deflocation mine-U05 (mine)
  (:exits ((mine-portal :west mine-T05 :east mine-V05))))

(deflocation mine-V05 (mine)
  (:exits ((mine-portal :west mine-U05 :south mine-V06))))

(deflocation mine-C06 (mine)
  (:exits ((mine-portal :north mine-C05 :south mine-C07))))

(deflocation mine-D06 (mine)
  (:exits ((mine-portal :east mine-E06))))

(deflocation mine-E06 (mine)
  (:exits ((mine-portal :west mine-D06 :east mine-F06))))

(deflocation mine-F06 (mine)
  (:exits ((mine-portal :west mine-E06 :east mine-G06))))

(deflocation mine-G06 (mine)
  (:exits ((mine-portal :west mine-F06 :north mine-G05 :south mine-G07 :east mine-H06))))

(deflocation mine-H06 (mine)
  (:exits ((mine-portal :west mine-G06 :east mine-I06))))

(deflocation mine-I06 (mine)
  (:exits ((mine-portal :west mine-H06 :east mine-J06-bottom))))

(deflocation mine-J06-top (mine)
  (:exits ((mine-portal :north mine-J05 :south mine-J07)
           (ladder :down mine-J06-bottom))))

(deflocation mine-J06-bottom (mine)
  (:exits ((mine-portal :west mine-I06 :east mine-K06)
           (ladder :up mine-J06-top))))

(deflocation mine-K06 (mine)
  (:exits ((mine-portal :west mine-J06-bottom :east mine-L06))))

(deflocation mine-L06 (mine)
  (:exits ((mine-portal :west mine-K06 :east mine-M06-top))))

(deflocation mine-M06-top (mine)
  (:exits ((mine-portal :west mine-L06)
           (ladder :down mine-M06-bottom))))

(deflocation mine-M06-bottom (mine)
  (:exits ((mine-portal :northeast mine-N05 :southeast mine-N07)
           (ladder :up mine-M06-top))))

(deflocation mine-P06 (mine)
  (:exits ((mine-portal :north mine-P05 :south mine-P07))))

(deflocation mine-T06 (mine)
  (:exits ((mine-portal :north mine-T05))))

(deflocation mine-V06 (mine)
  (:exits ((mine-portal :north mine-V05 :south mine-V07))))

(deflocation mine-C07 (mine)
  (:exits ((mine-portal :north mine-C06 :southeast mine-D08))))

(deflocation mine-G07 (mine)
  (:exits ((mine-portal :southwest mine-F08 :north mine-G06))))

(deflocation mine-J07 (mine)
  (:exits ((mine-portal :north mine-J06-top :south mine-J08))))

(deflocation mine-N07 (mine)
  (:exits ((mine-portal :northwest mine-M06-bottom :east mine-O07))))

(deflocation mine-O07 (mine)
  (:exits ((mine-portal :west mine-N07 :east mine-P07))))

(deflocation mine-P07 (mine)
  (:exits ((mine-portal :west mine-O07 :north mine-P06 :east mine-Q07))))

(deflocation mine-Q07 (mine)
  (:exits ((mine-portal :west mine-P07 :east mine-R07))))

(deflocation mine-R07 (mine)
  (:exits ((mine-portal :west mine-Q07 :south mine-R08))))

(deflocation mine-V07 (mine)
  (:exits ((mine-portal :southwest mine-U08 :north mine-V06))))

(deflocation mine-D08 (mine)
  (:exits ((mine-portal :northwest mine-C07 :east mine-E08))))

(deflocation mine-E08 (mine)
  (:exits ((mine-portal :west mine-D08 :south mine-E09 :east mine-F08))))

(deflocation mine-F08 (mine)
  (:exits ((mine-portal :west mine-E08 :northeast mine-G07))))

(deflocation mine-J08 (mine)
  (:exits ((mine-portal :north mine-J07 :south mine-J09))))

(deflocation mine-O08 (mine)
  (:exits ((mine-portal :south mine-O09 :east mine-P08))))

(deflocation mine-P08 (mine)
  (:exits ((mine-portal :west mine-O08 :east mine-Q08))))

(deflocation mine-Q08 (mine)
  (:exits ((mine-portal :west mine-P08 :east mine-R08))))

(deflocation mine-R08 (mine)
  (:exits ((mine-portal :west mine-Q08 :north mine-R07 :east mine-S08))))

(deflocation mine-S08 (mine)
  (:exits ((mine-portal :west mine-R08 :east mine-T08-top))))

(deflocation mine-T08-top (mine)
  (:exits ((mine-portal :west mine-S08) (ladder :down mine-T08-bottom))))

(deflocation mine-T08-bottom (mine)
  (:exits ((mine-portal :east mine-U08) (ladder :up mine-T08-top))))

(deflocation mine-U08 (mine)
  (:exits ((mine-portal :west mine-T08-bottom :northeast mine-V07))))

(deflocation mine-E09 (mine)
  (:exits ((mine-portal :north mine-E08))))

(deflocation mine-J09 (mine)
  (:name "Mine Entrance"
   :description "This is the entrance to an abandoned mine. A narrow opening,
     once boarded up, leads south toward the forest. The floor slants steeply
     down to the north, into the hillside."
   :exits ((mine-portal :north mine-J08)
           (narrow-opening :south jade.silverwood::forest-F00))))

(deflocation mine-O09 (mine)
  (:exits ((mine-portal :north mine-O08 :east mine-P09))))

(deflocation mine-P09 (mine)
  (:exits ((mine-portal :west mine-O09 :east mine-Q09))))

(deflocation mine-Q09 (mine)
  (:exits ((mine-portal :west mine-P09))))

;;; vaulted-hall

(defentity vaulted-hall (mine)
  (:name "Vaulted Hall"))

(deflocation vaulted-hall-T01 (vaulted-hall)
  (:exits ((hall-portal :south vaulted-hall-T02))))

(deflocation vaulted-hall-T02 (vaulted-hall)
  (:exits ((cell-door :west cell-S02)
           (hall-portal :north vaulted-hall-T01 :south vaulted-hall-T03))))

(deflocation vaulted-hall-T03 (vaulted-hall)
  (:exits ((hall-portal :north vaulted-hall-T02)
           (mine-portal :south guard-station-T04))))

;;; murky-pool

(defentity murky-pool (location)
  (:name "Murky Pool"
   :domain :underground
   :surface :water))

(deflocation murky-pool-G02 (murky-pool)
  (:exits ((cave-portal :west mushroom-cave-F02 :north mushroom-cave-G01
                        :east mushroom-cave-H02))))

;;; cell

(defentity cell (mine)
  (:name "Putrid Cell"))

(deflocation cell-S02 (cell)
  (:exits ((cell-door :east vaulted-hall-T02))))

;;; guard-station

(defentity guard-station (mine)
  (:name "Guard Station"))

(deflocation guard-station-T04 (guard-station)
  (:exits ((mine-portal :north vaulted-hall-T03 :south mine-T05))))
