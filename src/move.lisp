;;;; Implementation of movement between locations.

(in-package :jade)

;;;

(defgeneric exit-location (actor location exit &key force)
  (:documentation "Called when `actor' attempts to leave `location' via `exit'.
If `force' is true, do not allow observers to disallow the action."))

(defmethod exit-location :around (actor location exit &key force)
  (let ((observers (observer-list* actor exit location (? location :contents)))
        (message (exit-message actor exit)))
    (when (and (or force
                   (observers-allow observers :allow-exit-location actor location exit))
               (exit-combat actor :force force))
      (notify-observers observers :before-exit-location actor location exit)
      (show-observers (cons actor (cddr observers)) message) ; ignoring exit
      (call-next-method)
      (notify-observers observers :after-exit-location actor location exit)
      t)))

(defmethod exit-location (actor location exit &key force)
  (declare (ignore force))
  (cancel-current-activity actor)
  (deletef (? location :contents) actor)
  ;; NOTE: This doesn't set (entity-container actor) to nil, since it should
  ;; still be set in exit-world if applicable. It will either be reset upon
  ;; entering a new location, or unused after exit-world.
  (for-avatars-in (avatar location)
    (remove-neighbor avatar actor)))

(defmethod exit-location ((actor avatar) location exit &key force)
  (declare (ignore force))
  (reject-pending-offer actor)
  (when-let ((message (? exit #'exit-portal :transit-message)))
    (show actor message))
  (call-next-method))

;;;

(defgeneric exit-world (actor)
  (:documentation "Called when `actor' attempts to leave the game, after having
already left its location."))

(defmethod exit-world :around (actor)
  (observe-event actor :before-exit-world)
  (call-next-method)
  (observe-event actor :after-exit-world))

(defmethod exit-world (entity)
  (when-let ((allocator (? entity :allocator)))
    (deletef (? (location entity) :contents) entity)
    (funcall allocator :release)))

;;;

(defgeneric enter-world (actor)
  (:documentation "Called when `actor' attempts to enter the game, before
entering its initial location."))

(defmethod enter-world :around (actor)
  (observe-event actor :before-enter-world)
  (call-next-method)
  (observe-event actor :after-enter-world))

(defmethod enter-world (actor))

;;;

(defgeneric enter-location (actor location entry)
  (:documentation "Called when `actor' enters `location' via `entry'. There is
no allow phase."))

(defmethod enter-location :around (actor location entry)
  (let ((observers (list* actor entry location (? location :contents)))
        (message (entry-message actor entry)))
    (notify-observers observers :before-enter-location actor location entry)
    (call-next-method)
    (show-observers (cddr observers) message)  ; ignoring actor and entry
    (notify-observers observers :after-enter-location actor location entry)))

(defmethod enter-location (actor location entry)
  (for-avatars-in (avatar location)
      (update-neighbor avatar actor))
  (push actor (? location :contents))
  (setf (entity-container actor) location))

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
