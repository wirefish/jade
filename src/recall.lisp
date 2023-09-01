(in-package :jade)

(defentity spiritstone ()
  (:brief "a spiritstone"))

;;; Attune to a spiritstone.

(defclass attunement (activity) ())

(defparameter *attunement-duration* 6)

(defmethod begin-activity :around (actor (activity attunement))
  (cond
    ((deadp actor)
     (show actor "You are currently too dead to attune to anything."))
    ((battle actor)
     (show actor "You cannot attune while in combat."))
    ((some #`(entity-isa % 'spiritstone) (? (location actor) :contents))
     (call-next-method))
    (t
     (show actor "There is no spiritstone here."))))

(defmethod begin-activity (actor (activity attunement))
  (let* ((location (location actor))
         (contents (? location :contents))
         (observers (cons location contents)))
    (when (observers-allow observers :allow-attune actor)
      (show-message contents actor "begins to attune to the spiritstone.")
      (notify-observers observers :before-attune actor)
      (start-casting actor *attunement-duration*)
      (with-delay (*attunement-duration*)
        (finish-activity actor activity)))))

(defmethod finish-activity (actor (activity attunement))
  (let* ((location (location actor))
         (contents (? location :contents)))
    (stop-casting actor)
    (show-message contents actor "finishes attuning to the spiritstone.")
    (setf (? actor :recall-location) (entity-label location))
    (show-notice actor "You have attuned to ~a." (? location :name))
    (notify-observers (cons location contents) :after-attune actor)))

(defmethod cancel-activity (actor (activity attunement))
  (stop-casting actor)
  (show actor "Your attempt to attune has been interrupted."))

(defcommand attune (actor "attune")
  "Bind your spirit to a nearby spiritstone. The `help:recall` command returns
your body to the location of the stone to which you last attuned. If you die,
you can choose to resurrect at that same location."
  (begin-activity actor (make-instance 'attunement)))

;;; Recall to the location of the most-recently attuned spiritstone.

(defclass recall (activity) ())

(defparameter *recall-duration* 6)

(defmethod begin-activity :around (actor (activity recall))
  (cond
    ((battle actor)
     (show actor "You cannot recall while in combat."))
    ((not (symbol-value-as 'location (? actor :recall-location) nil))
     (show actor "You have not attuned to any spiritstone."))
    (t
     (call-next-method))))

(defmethod begin-activity (actor (activity recall))
  (let* ((location (location actor))
         (contents (? location :contents))
         (observers (cons location contents)))
    (when (observers-allow observers :allow-recall actor)
      (show-message contents actor "begins to cast recall.")
      (notify-observers observers :before-recall actor)
      (start-casting actor *recall-duration*)
      (with-delay (*recall-duration*)
        (finish-activity actor activity)))))

(defmethod finish-activity (actor (activity recall))
  (let* ((location (location actor))
         (contents (? location :contents)))
    (stop-casting actor)
    (show-message contents actor "finishes casting recall.")
    (notify-observers (cons location contents) :after-recall actor)
    (if-let ((dest (symbol-value-as 'location (? actor :recall-location) nil)))
      (when (exit-location actor location nil)
        (enter-location actor dest nil))
      (show actor "Something appears to have gone wrong."))))

(defmethod cancel-activity (actor (activity recall))
  (stop-casting actor)
  (show actor "Your attempt to recall has been interrupted."))

(defcommand recall (actor "recall")
  "Recall to the location of the spiritstone to which your spirit has most
recently been bound by using the `help:attune` command."
  (begin-activity actor (make-instance 'recall)))
