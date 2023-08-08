;;;; Functions used to send messages to update the state of the client.

(in-package :jade)

#|

The client UI state comprises the the following components:

- Avatar state (across the top): icon, name, level, race, auras,
  health/energy/xp bars.

- A list of neighboring entities (left column): each has icon, brief, and
  optional health bar and auras.

- Map (upper right): location name, region and subregion names, grid of map
  cells.

- Inventory: each item has icon, brief, quantity, group, subgroup.

- Equipment: each item has icon, brief, slot.

- Combat stats: names and values.

- Skills: unspent karma, each skill has name, rank, max rank.

- Chat: stream of text messages from other players.

- Location: name, description, (contents?), exits.

- Primary text: stream of text messages describing events, NPC interactions, etc.

- Activity status: either a castbar (for non-modal activities) or specific state
  for each modal activity such as combat or crafting.

All updates for an avatar are collected and then sent as a single message.

|#

(defun send-message (avatar message)
  "Sends a message to the socket associated with the avatar. If the avatar is
not connected (i.e. its socket is nil) then the message is queued so it can be
sent later when `connect-session' is called."
  (let ((data (websocket-encode-message message))
        (socket (avatar-socket avatar)))
    (if socket
        (as:write-socket-data socket data)
        (queue-push data (avatar-output-queue avatar)))))

(defun send-client-command (avatar command &rest args)
  "Sends a message that contains a JSON array whose first element is a command
name and whose subsequent elements are arguments to that command."
  (let ((message (with-output-to-string (s) (encode-json #h(:fn command :args args) s))))
    (send-message avatar message)))

;;; Functions that display formatted text in the main text pane.

(defun format-text (control-string args)
  (if args (apply #'format nil control-string args) control-string))

(defun show-text (target client-fn control-string &optional args)
  (when (typep target 'avatar)
    (send-client-command target client-fn (format-text control-string args))))

(defun show (target control-string &rest args)
  (show-text target "showText" control-string args))

(defun show-notice (target control-string &rest args)
  (show-text target "showNotice" control-string args))

(defun show-error (target control-string &rest args)
  (show-text target "showError" control-string args))

(defun show-raw (target control-string &rest args)
  (show-text target "showRaw" control-string args))

(defun show-tutorial-message (target message)
  (show-text target "showTutorial" message))

(defun show-help (target message)
  (show-text target "showHelp" message))

(defun show-links (avatar heading prefix links)
  (when links
    (send-client-command avatar "showLinks" heading prefix links)))

(defun tell (speaker target control-string &rest args)
  (when (typep target 'avatar)
    (send-client-command
     target
     "showSay"
     (format nil "~a says" (describe-brief speaker :capitalize t :article :definite))
     (format-text control-string args))))

;;; Functions that update various UI elements.

(defun update-avatar (avatar &rest properties)
  (send-client-command avatar "updateAvatar" (plist-hash-table properties)))

(defun update-neighbor (avatar obj &rest properties)
  ;; FIXME:
  (declare (ignore avatar obj properties)))

(defun show-location (location avatar)
  (send-client-command
   avatar "showLocation"
   (? location :name)
   (? location :description)
   (loop for exit in (? location :exits)
         when t  ; FIXME: (visiblep exit avatar)
           collect (exit-dir exit))
   (loop for obj in (? location :contents)
         when (and (not (eq obj avatar)) (not (? obj :implicit)))  ; FIXME: (visiblep obj viewer))
           collect (list (entity-id obj) (describe-brief obj) (describe-pose obj)))))


#|
(defun show-announce (location control-string &rest args)
  (let ((message (format-text control-string args)))
    (dolist (x (contents location))
      (show-notice x message))))

;;; Functions that manage the pane which displays entities in the same location
;;; as the player, aka neighbors.

(defgeneric neighbor-properties (obj)
  (:documentation "Returns a hash table of properties describing a neighbor, to
    be sent to the client."))

(defmethod neighbor-properties ((obj entity))
  (plist-hash-table (list :key (entity-id obj)
                          :brief (describe-brief obj :article nil)
                          :icon (describe-icon obj))))

(defun show-neighbors (avatar)
  (when-let ((session (get-session avatar)))
    (send-client-command
     session "setNeighbors"
     (mapcar #'neighbor-properties
             (remove-if #'(lambda (x)
                            (or (eq x avatar) (not (visiblep x avatar))))
                        (contents (location avatar)))))))

(defun update-neighbor (avatar obj &rest properties)
  (when-let ((session (get-session avatar)))
    (send-client-command session "updateNeighbor"
                         (if properties
                             (plist-hash-table (list* :key (id obj) properties))
                             (neighbor-properties obj))
                         nil)))

(defun remove-neighbor (avatar obj &optional message)
  (when-let ((session (get-session avatar)))
    (send-client-command session "removeNeighbor"
                         (id obj) message)))

;;; Functions that manage the equipment pane.

(defun update-equipment (avatar slots)
  (when-let ((session (get-session avatar)))
    (with-slots (equipment) avatar
      (send-client-command
       session "updateEquipment"
       (alist-hash-table
        (loop for slot in slots
              collect (cons slot (when-let ((item (gethash slot equipment)))
                                   (list (describe-icon item)
                                         (describe-brief item :article nil))))))))))
|#

;;; Cast bar for a non-modal activity.

(defun start-casting (avatar duration)
  (when-let ((session (get-session avatar)))
    (send-client-command session "startPlayerCast" duration)))

(defun stop-casting (avatar)
  (when-let ((session (get-session avatar)))
    (send-client-command session "stopPlayerCast")))

;;; Map.

(defun show-map (avatar &key (radius 3))
  (let* ((origin (entity-container avatar))
         (z-offset (? origin :z-offset))
         (map (walk-map origin radius :observer avatar)))
    (apply #'send-client-command
           avatar "showMap"
           (? origin :name)
           (or (? origin :region) "")
           (or (? origin :subregion) "")
           radius
           (loop for (x y z location) in map
                 if (eql (? location :z-offset) z-offset)
                   collect (list x y
                                 (? location :name)
                                 (or (? location :icon) "")
                                 (or (? location :surface) "")
                                 (or (? location :surround) "")
                                 (or (? location :domain) "")
                                 (location-state location avatar))))))
