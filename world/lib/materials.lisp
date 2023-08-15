(in-package :jade.lib)

;;; Cloth

(defmaterial cloth ()
  (:name "cloth"
   :tags (cloth)))

(defmaterial cotton (cloth)
  (:name "cotton"))

(defmaterial canvas (cloth)
  (:name "canvas"))

;;; Leather

(defmaterial leather ()
  (:name "leather"
   :tags (leather)))

(defmaterial worn-leather (leather)
  (:name "worn leather"))

(defmaterial thin-leather (leather)
  (:name "thin leather"))

(defmaterial thick-leather (leather)
  (:name "thick leather"))

;;; Metal

(defmaterial metal ()
  (:name "metal"
   :tags (metal)))

(defmaterial soft-metal (metal)
  (:name "soft metal"
   :tags (metal soft)
   :durability 0.5))

(defmaterial hard-metal (metal)
  (:name "hard metal"
   :tags (metal hard)))

(defmaterial copper (soft-metal)
  (:name "copper"
   :level 1))

(defmaterial silver (soft-metal)
  (:name "silver"
   :level 10))

(defmaterial gold (soft-metal)
  (:name "gold"
   :level 20
   :traits (:precision 1)
   :level 20))
