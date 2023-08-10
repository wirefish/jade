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
  ;; FIXME: (stop-activity actor)
  (remove actor (? location :contents))
  (setf (entity-container actor) nil)
  (for-avatars-in (avatar location)
    (remove-neighbor avatar actor)))

(defmethod exit-location ((actor avatar) location exit &key force)
  (declare (ignore force))
  ;; FIXME: check for exit message.
  (reject-offer actor)
  (cancel-current-activity actor)
  (when exit
    (show actor "You head ~a." (direction-name (exit-dir exit))))
  (call-next-method))

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
  (call-next-method))

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
  (push actor (? location :contents))
  (setf (entity-container actor) location)
  (for-avatars-in (avatar location)
    (unless (eq avatar actor)
      (update-neighbor avatar actor))))

(defmethod enter-location ((actor avatar) location entry)
  (call-next-method)
  (show-location actor)
  (show-map actor)
  (show-neighbors actor)
  (when-let ((msg (? location :tutorial)))
    (maybe-show-tutorial actor (entity-label location) msg)))

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

(defcommand move (actor "go" direction)
  "Move in a specified direction; for example, `go north`. See `help:movement`
for more information."
  (let* ((location (entity-container actor))
         (matches (find-matches direction
                                (? location :contents)
                                (? location :exits))))
    (case (length matches)
      (0 (show actor "You can't go that way."))
      (1 (traverse-portal actor location (first matches)))
      (otherwise (show actor "Do you want to go ~a?"
                       (format-list #'describe-brief matches "or"))))))

(maphash-keys #'(lambda (dir)
                  (let ((command (format nil "go ~a" (direction-name dir))))
                    (make-alias (direction-name dir) command)
                    (when-let ((abbrev (direction-abbrev dir)))
                      (make-alias abbrev command))))
              *directions*)
