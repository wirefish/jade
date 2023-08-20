(in-package :jade)

;;; Basic random number generation.

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

;;; Wrappers around cl-async delays and intervals to support random timing.

(defmacro with-random-delay ((min max) &body body)
  `(as:with-delay ((random-integer ,min ,max))
     ,@body))

(defun random-interval (callback &key min max event-cb)
  (let (event)
    (labels ((main ()
               (funcall callback)
               (when event
                 (setf event (as:delay #'main :time (random-integer min max)
                                              :event-cb event-cb)))))
      (setf event (as:delay #'main :time (random-integer min max)
                                   :event-cb event-cb))
      (lambda ()
        (when event
          (as:remove-event event)
          (setf event nil))))))

(defmacro with-random-interval ((min max) &body body)
  `(random-interval (lambda () ,@body) :min ,min :max ,max))
