(in-package :jade)

;;; Talk to an NPC.

(defun talkative (entity)
  (reacts-to-event-p entity :when-talk))

(defgeneric talk (actor target topic)
  (:documentation "Called when `actor' talks to `target' about `topic'."))

(defmethod talk :around (actor target topic)
  ;; Assume that actor and target are both contained within actor's location.
  (if (talkative target)
      (let ((observers (list* (location actor) (? (location actor) :contents))))
        (when (observers-allow-p observers :allow-talk actor target topic)
          (notify-observers observers :before-talk actor target topic)
          (call-next-method)
          (notify-observers observers :after-talk actor target topic)))
      (show actor "You cannot talk to ~a." (describe-brief target :article :definite))))

(defmethod talk (actor target topic)
  (observe-event target :when-talk actor target topic))

(defcommand talk (actor "talk" "to" target "about" topic)
  "Talk to someone (or something!) at your location. You may optionally specify a
particular topic of interest."
  (let* ((candidates (remove actor (? (location actor) :contents)))
         (targets (if target
                      (find-matches target candidates)
                      (delete-if-not #'talkative candidates))))
    (case (length targets)
      (0 (show actor (if target
                         (format nil "You don't see anyone matching \"~a\" to talk to."
                                 (join-tokens target))
                         "You don't see anyone to talk to.")))
      (1 (talk actor (first targets) topic))
      (t (show actor "Do you want to talk to ~a?"
               (format-list #'describe-brief targets "or"))))))

;;; Say something to everyone at the same location.

(defun punct-char-p (c)
  (not (or (alpha-char-p c) (digit-char-p c))))

(defun format-message (words)
  "Formats a raw sequence of words for presentation to an avatar."
  (let ((message (format nil "~{~a~^ ~}" words)))
    (setf (char message 0) (char-upcase (char message 0)))
    (if (not (punct-char-p (char message (1- (length message)))))
        (concatenate 'string message ".")
        message)))

(defgeneric say (actor message location)
  (:method :around (actor message location)
    (let ((observers (list* location (contents location))))
      (when (observers-allow observers :allow-say actor message)
        (notify-observers observers :before-say actor message)
        (call-next-method)
        (notify-observers observers :after-say actor message))))
  (:method (actor message location)
    (dolist (observer (contents location))
      (if (eq observer actor)
          (show observer "You say, ~s" message)
          (show observer "~a says, ~s" (describe-brief actor :capitalize t))))))

(defcommand (actor ("say" "\"") :rest words)
  (say actor (format-message words) (location actor)))

;;; TODO: Yell something that can be heard at your location and nearby locations.

;;; TODO: Tell something to a specific player.
