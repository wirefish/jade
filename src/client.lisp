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
        (progn (as:write-socket-data socket data) t)
        (progn (queue-push data (avatar-output-queue avatar)) nil))))

(defun send-queued-messages (avatar)
  "Sends any queued messages to `avatar'. This is generally called after the
player reconnects while the avatar is already in the world."
  (when-let ((socket (avatar-socket avatar)))
    (loop while (not (queue-empty (avatar-output-queue avatar))) do
      (let ((data (queue-pop (avatar-output-queue avatar))))
        (as:write-socket-data socket data)))))

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

(defun show-near (actor control-string &rest args)
  (for-avatars-in (avatar (location actor))
    (apply #'show avatar control-string args)))

(defun show-notice (target control-string &rest args)
  (show-text target "showNotice" control-string args))

(defun show-error (target control-string &rest args)
  (show-text target "showError" control-string args))

(defun show-raw (target control-string &rest args)
  (show-text target "showRaw" control-string args))

(defun show-tutorial (target message)
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
     (describe-brief speaker :capitalize t :article :definite)
     "says"
     (format-text control-string args))))

(defun announce (origin radius control-string &rest args)
  (let ((message (format-text control-string args)))
    (for-avatars-near (avatar origin radius)
      (show-notice avatar message))))

;;; Update elements of the avatar status bar on top of the screen.

(defparameter *avatar-attributes*
  #h(:name (lambda (a) (? a :name))
     :race (lambda (a) (describe-brief (? a :race) :article nil))
     :icon #'get-icon
     :level (lambda (a) (? a :level))
     :health (lambda (a) (? a :health))
     :max-health (lambda (a) (? a :max-health))
     :energy (lambda (a) (? a :energy))
     :max-energy (lambda (a) (? a :max-energy))
     :xp (lambda (a) (? a :xp))
     :max-xp (lambda (a) (xp-required-for-level (1+ (? a :level))))))

(defun update-avatar (avatar &rest keys)
  (with-slots (client-state) avatar
    (let (updates)
      (loop for key in (or keys (hash-table-keys *avatar-attributes*)) do
        (let ((value (funcall (gethash key *avatar-attributes*) avatar))
              (client-key (cons :avatar key)))
          (unless (eql value (gethash client-key client-state))
            (sethash client-key client-state value)
            (push (cons key value) updates))))
      (when updates
        (send-client-command avatar "updateAvatar" (alist-hash-table updates))))))

;;; Manage the neighbors panel.

(defmethod neighbor-properties (entity)
  (let ((properties #h(:key (entity-id entity)
                       :brief (describe-brief entity :article nil)
                       :icon (get-icon entity))))
    (when-let ((max-health (? entity :max-health))
               (health (? entity :health)))
      (sethash* properties :health health :max-health max-health))
    properties))

(defun show-neighbors (avatar)
  "Displays all properties for neighbors visible to `avatar'."
  (send-client-command
   avatar "setNeighbors"
   (loop for entity in (? (location avatar) :contents)
         unless (or (eq entity avatar) (? entity :implicit) (? entity :implicit-neighbor)
                    (not (can-see avatar entity)))
           collect (neighbor-properties entity))))

(defun update-neighbor (avatar neighbor &rest properties)
  "Updates specific properties of `neighbor' as seen by `avatar'. If
`properties' is nil, updates all properties."
  (when (can-see avatar neighbor)
    (send-client-command
     avatar "updateNeighbor"
     (if properties
         (plist-hash-table (list* :key (entity-id neighbor) properties))
         (neighbor-properties neighbor)))))

(defun remove-neighbor (avatar neighbor &optional message)
  (send-client-command avatar "removeNeighbor" (entity-id neighbor) message))

;;; Describe the current location.

(defun show-location (avatar &optional location)
  (let ((location (or location (entity-container avatar))))
    (send-client-command
     avatar "showLocation"
     (update-client-state avatar :location-name (? location :name))
     (update-client-state avatar :location-description (? location :description))
     (loop for exit in (? location :exits)
           when (can-see avatar exit)
             collect (exit-dir exit))
     (loop for obj in (? location :contents)
           unless (or (eq obj avatar) (? obj :implicit) (not (can-see avatar obj)))
             collect (list (entity-id obj) (describe-brief obj) (describe-pose obj))))))

;;;

(defun show-vendor-items (avatar header vendor verb items)
  (send-client-command
   avatar "showVendorItems" header
   (describe-brief vendor)
   verb
   (loop for item in items
         collect (list (describe-brief item :quantity 1 :article nil)
                       (describe-brief (? item :price))
                       (get-icon item)))))

;;;

(defun show-trainer-skills (avatar header trainer skills)
  (send-client-command
   avatar "showTrainerSkills" header
   (describe-brief trainer)
   (loop for skill in skills
         collect (with-slots (label name summary price karma) skill
                   (list name
                         summary
                         (when price (describe-brief price))
                         karma
                         (and (gethash label (skills avatar)) t))))))

;;;

(defun update-inventory (avatar added-items &optional removed-items)
  (let ((arg (make-hash-table :test #'equal)))
    (loop for item in added-items
          do (sethash (to-string (entity-id item)) arg
                      (list (get-icon item)
                            (describe-brief item :article nil)
                            (? item :item-sort-key))))
    (loop for item in removed-items
          do (sethash (to-string (entity-id item)) arg nil))
    (send-client-command avatar "updateInventory" arg)))

;;; Manage the equipment pane.

(defun update-equipment (avatar &optional (slots *equipment-slots*))
  (when-attributes (equipment) avatar
    (send-client-command
     avatar "updateEquipment"
     (alist-hash-table
      (loop for slot in slots
            collect (cons slot (when-let ((item (gethash slot equipment)))
                                 (list (get-icon item)
                                       (describe-brief item :article nil)))))))))

;;; Manage the combat pane.

(defun update-combat (avatar)
  (with-slots (cached-traits) avatar
    (bind ((attack (select-attack avatar nil))
           (min max (damage-range avatar attack)))
      (send-client-command
       avatar "updateCombat"
       (format nil "~1$" (attack-rating avatar attack))
       (format nil "~1$" (defense-rating avatar nil))
       (format nil "~1$" (or (? attack :speed) 5))
       (format nil "~1$" (/ (+ min max) 2))
       (mapcar (lambda (trait) (list trait (gethash trait cached-traits 0)))
               (hash-table-keys *combat-traits*))
       nil))))

;;; Manage the skills pane.

(defun update-skills (avatar &rest labels)
  (let ((labels (if labels labels (hash-table-keys (skills avatar)))))
    (apply #'send-client-command
     avatar "updateSkills"
     (karma avatar)
     (loop for label in labels
           collect (let ((skill (symbol-value label))
                         (rank (gethash label (skills avatar))))
                     (with-slots (name max-rank) skill
                       (if rank
                           (list label name (floor rank) max-rank)
                           (list label))))))))

;;;

(defun update-quests (avatar &rest labels)
  (let ((entries (if labels
                     (loop for label in labels
                           collect (or (assoc label (active-quests avatar))
                                       (list label)))
                     (active-quests avatar))))
    (apply #'send-client-command
     avatar "updateQuests"
     (loop for (label phase state) in entries
           collect (if phase
                       (let ((quest (symbol-value label)))
                         (list label
                               (quest-name quest)
                               (quest-level quest)
                               (quest-phase-summary (elt (quest-phases quest) phase))))
                       (list label))))))

;;; Cast bar for a non-modal activity.

(defun start-casting (avatar duration)
  (send-client-command avatar "startPlayerCast" duration))

(defun stop-casting (avatar)
  (send-client-command avatar "stopPlayerCast"))

;;; Map.

(defun show-map (avatar &key (radius 3))
  (let* ((origin (entity-container avatar))
         (z-offset (? origin :z-offset))
         (map (walk-map origin radius :observer avatar :min-exit-size +small+)))
    (apply #'send-client-command
           avatar "showMap"
           (? origin :name)
           (or (? origin :region :name) "")
           (or (? origin :subregion) "")
           radius
           (loop for (x y z location) in map
                 when (eql (? location :z-offset) z-offset)
                   collect (list x y
                                 (? location :name)
                                 (or (? location :icon) "")
                                 (or (? location :surface) "")
                                 (or (? location :surround) "")
                                 (or (? location :domain) "")
                                 (location-state location avatar))))))

(defun update-ui (avatar &key for-location)
  "Updates all client UI elements. Elements normally updated when entering a
location are updated only if `for-location' is true."
  (reset-client-state avatar)
  (update-avatar avatar)
  (update-equipment avatar)
  (update-inventory avatar (? avatar :inventory))
  (update-combat avatar)
  (update-skills avatar)
  (update-quests avatar)
  (when for-location
    (show-location avatar)
    (show-map avatar)
    (show-neighbors avatar)))
