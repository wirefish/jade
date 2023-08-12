(in-package :jade.lib)

;;; Cloth

(defmaterial cotton
  :name "cotton"
  :tags (cloth))

(defmaterial canvas
  :name "canvas"
  :tags (cloth))

;;; Leather

(defmaterial worn-leather
  :name "worn leather"
  :tags (leather))

;;; Metal

(defmaterial copper
  :name "copper"
  :tags (metal soft))

(defmaterial gold
  :name "gold"
  :adjective "golden"
  :tags (metal soft)
  :traits ((fire-resist -1) (charisma 1))
  :level 12)
