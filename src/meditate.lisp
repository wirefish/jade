(in-package :jade)

(defclass meditation (activity) ())

(defparameter *meditation-duration* 3)

(defmethod begin-activity (actor (activity meditation))
  (let ((msg (action-message actor "begins to meditate.")))
    (show-observers (list* (location actor) (? (location actor) :contents))
                    msg :before-meditate actor))
  (start-casting actor *meditation-duration*)
  (with-delay (*meditation-duration*)
    (finish-activity actor activity)))

(defmethod finish-activity (actor (activity meditation))
  (stop-casting actor)
  (let ((msg (action-message actor "finishes meditating.")))
    (show-observers (list* (location actor) (? (location actor) :contents))
                    msg :after-meditate actor)))

(defmethod cancel-activity (actor (activity meditation))
  (stop-casting actor)
  (show actor "Your meditation has been interrupted."))

(defcommand meditate (actor "meditate")
  "Spend a few moments focusing your mind. This action may have side effects
depending on your location."
  (begin-activity actor (make-instance 'meditation)))
