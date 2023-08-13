(in-package :jade.lib)

(defentity moving-location (location)
  (:route-exits nil)  ; list of (portal dir dest) as with :exits

  (:after-enter-world ()
    ;; Convert route from a list of (portal dir dest) to a circular list of
    ;; (location exit entry), where exit is to be added to location and entry is
    ;; to be added to self while self is "docked" at location.
    (setf (? self :route)
          (apply #'circular-list
                 (loop for (portal dir dest) in (? self :route-exits)
                       collect (let ((portal (clone-entity portal)))
                                 (list (symbol-value dest)
                                       (make-exit :dir dir :dest dest :portal portal)
                                       (make-exit :dir (direction-opposite dir)
                                                  :dest (entity-label self)
                                                  :portal portal))))))
    (with-delay (0)
      (observe-event self :arrive self)))

  (:before-exit-world ()
    (bind (((curr-location exit entry) (first (? self :route))))
      (deletef (? self :exits) exit)
      (deletef (? curr-location :exits) entry)))

  (:arrive (self)
    (with-attributes (route) self
      (bind ((prev-location (caar route))
             ((curr-location exit entry) (second route)))
        (push exit (? self :exits))
        (push entry (? curr-location :exits))
        (let ((message (format nil "~a arrives from ~a."
                               (describe-brief self :capitalize t)
                               (? prev-location :name))))
          (for-avatars-near (avatar curr-location 3)
            (show-notice avatar message)
            (show-map avatar)))))
    (pop (? self :route))
    (with-delay (15)
      (observe-event self :announce-departure self)))

  (:announce-departure (self)
    (with-attributes (route) self
      (let ((curr-location (caar route))
            (next-location (caadr route)))
        (announce curr-location 3 "~a is about to depart for ~a. All aboard!"
                  (describe-brief self :capitalize t)
                  (? next-location :name))))
    (with-delay (5)
      (observe-event self :depart self)))

  (:depart (self)
    (with-attributes (route) self
      (bind (((curr-location exit entry) (first route))
             (next-location (caadr route)))
        (announce curr-location 3 "~a departs for ~a."
                  (describe-brief self :capitalize t)
                  (? next-location :name))
        (deletef (? self :exits) exit)
        (deletef (? curr-location :exits) entry)
        (for-avatars-in (avatar self)
          (show-map avatar))
        (for-avatars-near (avatar curr-location 3)
          (show-map avatar))))
    (with-delay (3)
      (observe-event self :arrive self))))
