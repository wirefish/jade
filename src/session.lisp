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
