(in-package :jade)

;; A mapping from session keys to session objects.
(defvar *sessions* (make-hash-table :test #'equal))

(defun make-session-key ()
  (ironclad:byte-array-to-hex-string (ironclad:make-random-salt)))

;;; NOTE: The socket, session, and avatar have a number of references between
;;; themselves, as follows:
;;;
;;; socket -> session
;;; session -> socket and avatar
;;; avatar -> session

(defstruct session
  (key (make-session-key))
  account-id
  username
  remote-ip
  avatar
  location
  socket
  input-buffer
  (output-queue (make-queue)))

(defgeneric get-session (entity)
  (:documentation "Returns the session associated with `entity', if any.")
  (:method ((entity entity))
    nil)
  (:method ((entity avatar))
    (slot-value entity 'session)))

(defun connect-session (session socket)
  "Associates a socket with a session and sends any queued output messages."
  (setf (session-socket session) socket)
  ;; FIXME:? (setf (as:socket-data socket) session)
  (loop while (not (queue-empty (session-output-queue session))) do
       (let ((data (queue-pop (session-output-queue session))))
         (as:write-socket-data socket data))))

(defun disconnect-session (session)
  "Removes the references between `session' and the associated socket, if any."
  (let ((socket (session-socket session)))
    (when socket
      (setf (as:socket-data socket) nil)))
  (setf (session-socket session) nil))

(defun send-message (session message)
  "Sends a message to the socket associated with the session. If the session is
not connected (i.e. its socket is nil) then the message is queued so it can be
sent later when `connect-session' is called."
  (let ((data (websocket-encode-message message))
        (socket (session-socket session)))
    (if socket
        (as:write-socket-data socket data)
        (queue-push data (session-output-queue session)))))

(defun send-client-command (session command &rest args)
  "Sends a message that contains a JSON array whose first element is a command
name and whose subsequent elements are arguments to that command."
  (let ((message (with-output-to-string (s) (encode-json #h(:fn command :args args) s))))
    (if session
        (send-message session message)
        (print message))
    nil))

;;;; Functions used to send information to the client.

(defun format-text (control-string args)
  (if args (apply #'format nil control-string args) control-string))

(defun show (target control-string &rest args)
  (when-let ((session (get-session target)))
    (send-client-command session "showText" (format-text control-string args))))

(defun show-notice (target control-string &rest args)
  (when-let ((session (get-session target)))
    (send-client-command session "showNotice" (format-text control-string args))))

(defun show-error (target control-string &rest args)
  (when-let ((session (get-session target)))
    (send-client-command session "showError" (format-text control-string args))))

(defun show-raw (target control-string &rest args)
  (when-let ((session (get-session target)))
    (send-client-command session "showRaw" (format-text control-string args))))

(defun show-action (observer message &rest args)
  (when-let ((session (get-session observer)))
    (send-client-command session "showText" (apply message observer args))))

(defun show-observers (observers message event &rest args)
  (dolist (observer observers)
    (when observer
      (when message
        (apply #'show-action observer message args))
      (apply #'observe-event observer event args))))

(defun show-tutorial-message (target message)
  (when-let ((session (get-session target)))
    (send-client-command session "showTutorial" message)))

(defun show-help (target message)
  (when-let ((session (get-session target)))
    (send-client-command session "showHelp" message)))

(defun show-links (target heading prefix links)
  (when-let ((session (get-session target)))
    (send-client-command session "showLinks" heading prefix links)))

(defun update-avatar (avatar &rest properties)
  (when-let ((session (get-session avatar)))
    (send-client-command session "updateAvatar" (plist-hash-table properties))))

(defun update-neighbor (avatar obj &rest properties)
  ;; FIXME:
  (declare (ignore avatar obj properties)))

(defun show-location (location viewer)
  "Shows a description of `location' to `viewer'."
  (when-let ((session (get-session viewer)))
    (send-client-command
     session
     "showLocation"
     (? location :name)
     (? location :description)
     (loop for exit in (? location :exits)
           when t  ; FIXME: (visiblep exit viewer)
             collect (exit-dir exit))
     (loop for obj in (? location :contents)
           when (and (not (eq obj viewer)) (not (? obj :implicit)))  ; FIXME: (visiblep obj viewer))
             collect (list (entity-id obj) (describe-brief obj) (describe-pose obj))))))

#|
(defun tell (speaker target control-string &rest args)
  (when-let ((session (get-session target)))
    (send-client-command
     session
     "showSay"
     (format nil "~a says" (describe-brief speaker :capitalize t :article :definite))
     (format-text control-string args))))

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

;;;

(defun start-casting (avatar duration)
  (when-let ((session (get-session avatar)))
    (send-client-command session "startPlayerCast" duration)))

(defun stop-casting (avatar)
  (when-let ((session (get-session avatar)))
    (send-client-command session "stopPlayerCast")))
