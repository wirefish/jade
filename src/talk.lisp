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

(defun join-words (tokens)
  "Formats input tokens as words and punctuation with appropriate spacing."
  (labels ((punct-p (c) (position c "!?,.")))
    (apply #'strcat
     (loop for token in tokens for i from 0
           nconc (if (or (= i 0) (punct-p (char token 0)))
                     (list token)
                     (list " " token))))))

(defgeneric say (actor message))

(defmethod say :around (actor message)
  (let ((location (location actor)))
    (let ((observers (list* location (? location :contents))))
      (when (observers-allow-p observers :allow-say actor message)
        (notify-observers observers :before-say actor message)
        (call-next-method)
        (notify-observers observers :after-say actor message)))))

(defmethod say (actor message)
  (let ((actor-description (describe-brief actor :capitalize t)))
    (dolist (observer (? (location actor) :contents))
      (if (eq observer actor)
          (show observer "You say, ~s" message)
          (show observer "~a says, ~s" actor-description message)))))

(defcommand say (actor ("say") :rest message)
  "Say something to everyone in your location."
  (say actor (join-words message)))

;;; TODO: Yell something that can be heard at your location and nearby locations.

;;; TODO: Tell something to a specific player.
