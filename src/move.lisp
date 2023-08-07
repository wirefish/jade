;;;; Implementation of movement between locations.

(in-package :jade)

;;;

(defgeneric exit-location (actor location exit &key force)
  (:documentation "Called when `actor' attempts to leave `location' via `exit'. If `force' is
true, do not allow observers to disallow the action."))

(defmethod exit-location :around (actor location exit &key force)
  (let ((observers (list* location exit (? location :contents))))
    (when (or force
              (observers-allow-p observers :allow-exit-location actor location exit))
      (notify-observers observers :before-exit-location actor location exit)
      (call-next-method)
      (notify-observers observers :after-exit-location actor location exit)
      t)))

(defmethod exit-location (actor location exit &key force)
  (declare (ignore force))
  (remove actor (? location :contents)))

#|
(defmethod exit-location (actor location exit &key)
  (stop-activity actor)
  (remove-from-container location actor)
  (dolist (observer (contents location))
    (remove-neighbor observer actor)))
|#

;;;

(defgeneric exit-world (actor)
  (:documentation "Called when `actor' attempts to leave the game, after having already left its
location."))

(defmethod exit-world :around (actor)
  (observe-event actor :before-exit-world actor)
  (call-next-method)
  (observe-event actor :after-exit-world actor))

(defmethod exit-world (actor)
  ;;(stop-all-behaviors actor)
  )

;;;

(defgeneric enter-world (actor)
  (:documentation "Called when `actor' attempts to enter the game, before
entering its initial location."))

(defmethod enter-world :around (actor)
  (observe-event actor :before-enter-world actor)
  (call-next-method)
  (observe-event actor :after-enter-world actor))

(defmethod enter-world (actor)
  ;; FIXME: start required behaviors?
  nil)

(defmethod enter-world ((actor avatar))
  ;; FIXME: start regen behavior
  (with-attributes (name race icon level xp) actor
    (update-avatar
     actor
     :name name
     :race (describe-brief race :article nil)
     :icon icon
     :level level
     :xp xp
     :max-xp (xp-required-for-level (1+ level)))))

;;;

(defgeneric enter-location (actor location entry)
  (:documentation "Called when `actor' enters `location' via `entry'. There is
  no allow phase."))

(defmethod enter-location :around (actor location entry)
  (let ((observers (list* actor location entry (? location :contents))))
    (notify-observers observers :before-enter-location actor location entry)
    (call-next-method)
    (notify-observers observers :after-enter-location actor location entry)))

(defmethod enter-location (actor location entry)
  (dolist (observer (? location :contents))
    (update-neighbor observer actor))
  (push actor (? location :contents))
  (setf (entity-container actor) location))

(defmethod enter-location ((actor avatar) location entry)
  (call-next-method)
  (show-location location actor))

#| FIXME:
  (when-let ((msg (? location :tutorial)))
    (show-tutorial actor (entity-label location) msg))
  (show-map actor 3)
(show-neighbors actor))
|#

;;; Combine exit-location and enter-location.

(defun find-exit (location direction)
  (find-if #'(lambda (exit) (eq (exit-dir exit) direction))
           (? location :exits)))

(defun find-entry (location exit)
  (find-exit location (direction-opposite (exit-dir exit))))

(defun traverse-portal (actor location exit)
  (if-let ((dest (find-location (and exit (exit-dest exit)))))
    (let ((entry (find-entry dest exit)))
      (when (exit-location actor location exit)
        (enter-location actor dest entry)))
    (show actor "A mysterious force prevents you from going that way.")))

;;;

#|
(defcommand (actor "go" direction)
  "Move in a specified direction; for example, `go north`. See `help:movement`
  for more information."
  (let ((matches (find-matches direction
                               (contents (location actor))
                               (exits (location actor)))))
    (case (length matches)
      (0 (show actor "You can't go that way."))
      (1 (traverse-portal actor (location actor) (first matches)))
      (otherwise (show actor "Do you want to go ~a?"
                       (format-list #'describe-brief matches))))))

(maphash-keys #'(lambda (dir)
                  (let ((command (format nil "go ~a" (direction-name dir))))
                    (make-alias (direction-name dir) command)
                    (when-let ((abbrev (direction-abbrev dir)))
                      (make-alias abbrev command))))
              *directions*)
|#
