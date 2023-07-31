(in-package :jade.isle-of-dawn)

(deflocation hilltop ()
  (:name "Hilltop")

  (:allow-enter ((actor avatar) location entry)
    (jade::show actor "Hello!")
    (print 1234)
    (disallow-action)))
