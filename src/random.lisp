(in-package :jade)

(defun round-random (n)
  "Returns a random integer that is either floor(n) or ceil(n), with probability
  weighted by the fractional part of n."
  (if (<= (mod n 1) (random 1.0)) (floor n) (ceiling n)))

(defun random-range (min max)
  "Returns a random number uniformly distributed in the range [min, max)."
  (let ((min (float min))
        (max (float max)))
    (+ min (random (- max min)))))
