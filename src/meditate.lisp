(in-package :jade)

(defclass meditation (activity) ())

(defparameter *meditation-duration* 3)

(defmethod begin-activity (actor (activity meditation))
  (let ((location (location actor)))
    (show-message (? location :contents) actor "begins to meditate.")
    (notify-observers (list* location (? location :contents))
                      :before-meditate actor)
  (start-casting actor *meditation-duration*)
  (with-delay (*meditation-duration*)
    (finish-activity actor activity))))

(defmethod finish-activity (actor (activity meditation))
  (let ((location (location actor)))
    (stop-casting actor)
    (show-message (? location :contents) actor "finishes meditating.")
    (notify-observers (list* location (? location :contents))
                      :after-meditate actor)))

(defmethod cancel-activity (actor (activity meditation))
  (stop-casting actor)
  (show actor "Your meditation has been interrupted."))

(defcommand meditate (actor "meditate")
  "Spend a few moments focusing your mind. This action may have side effects
depending on your location."
  (begin-activity actor (make-instance 'meditation)))
