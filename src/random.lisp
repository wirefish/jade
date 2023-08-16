(in-package :jade)

(declaim (ftype (function (float) integer) round-random))
(defun round-random (n)
  "Returns a random integer that is either floor(n) or ceil(n), with probability
  weighted by the fractional part of n."
  (multiple-value-bind (i frac) (floor n)
    (if (< (random 1.0) frac) (1+ i) i)))

(declaim (ftype (function (float float) float) random-float))
(defun random-float (min max)
  (+ min (random (- max min))))

(declaim (ftype (function (integer integer) integer) random-int))
(defun random-integer (min max)
  (+ min (random (1+ (- max min)))))
