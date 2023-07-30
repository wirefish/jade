(in-package :jade)

(defun round-random (n)
  "Returns a random integer that is either floor(n) or ceil(n), with probability
  weighted by the fractional part of n."
  (if (<= (mod n 1) (random 1.0)) (floor n) (ceiling n)))

(defun uniform-random (min max)
  "Returns an uniformly-distributed random integer between between `min` and
  `max`, inclusive."
  (declare (type integer min max))
  (+ min (random (1+ (- max min)))))
