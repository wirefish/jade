(in-package :jade)

(defclass meditation (activity) ())

(defparameter *meditation-duration* 3)

(defmethod begin-activity (actor (activity meditation))
  (let ((msg (format nil "~a begins to meditate." (describe-brief actor :capitalize t))))
    (show-observers (delete actor (list* (location actor) (? (location actor) :contents)))
                    msg
                    :before-meditate actor))
  (show actor "You begin to meditate.")
  (observe-event actor :before-meditate actor)
  (start-casting actor *meditation-duration*)
  (with-delay (*meditation-duration*)
    (finish-activity actor activity)))

(defmethod finish-activity (actor (activity meditation))
  (show actor "You finish your meditation.")
  (observe-event actor :after-meditate actor)
  (stop-casting actor)
  (let ((msg (format nil "~a finishes meditating." (describe-brief actor :capitalize t))))
    (show-observers (delete actor (list* (location actor) (? (location actor) :contents)))
                    msg
                    :after-meditate actor)))

(defmethod cancel-activity (actor (activity meditation))
  (stop-casting actor)
  (show actor "Your meditation has been interrupted."))

(defcommand meditate (actor "meditate")
  "Spend a few moments focusing your mind. This action may have side effects
depending on your location."
  (begin-activity actor (make-instance 'meditation)))
