(in-package :jade)

;;; Session keys.

(defparameter *signing-key* (ironclad:random-data 32)
  "A secret key used when signing and validating session keys.")

(defparameter *signature-length* 32
  "The length of the array returned by the `signature' function.")

(defparameter *session-key-valid-seconds* 30
  "The maximum allowed time between the generation of a session key and its
first use to create a new session.")

(defparameter *session-key-cookie* "jade-session-key"
  "The name of the cookie used to store the session key.")

(defun signature (data)
  (let ((mac (ironclad:make-mac :hmac *signing-key* :sha256)))
    (ironclad:update-mac mac data)
    (ironclad:produce-mac mac)))

(defun make-session-key (username account-id)
  (let* ((msg (write-to-string (list username account-id (get-universal-time))))
         (data (babel:string-to-octets msg :encoding :utf-8))
         (sig (signature data)))
    (cl-base64:usb8-array-to-base64-string (concatenate '(vector (unsigned-byte 8)) data sig))))

(defun validate-session-key (key)
  (ignore-errors
   (let* ((key (cl-base64:base64-string-to-usb8-array key))
          (sep (- (length key) *signature-length*))
          (msg (subseq key 0 sep)))
     (when (equalp (subseq key sep) (signature msg))
       (let ((msg (babel:octets-to-string msg :encoding :utf-8)))
         (read-from-string msg))))))

(defun get-session-key (request)
  (http-request-get-cookie-value *session-key-cookie* request))

;;;

(defvar *avatars* (make-hash-table)
  "A mapping from account-id to avatar object for all avatars present in the
world.")

(defun setup-session (avatar socket request)
  "Initializes the relationship between an avatar and a new websocket."
  (setf (avatar-socket avatar) socket)
  (setf (avatar-remote-ip avatar) (http-request-get-header request "X-Real-IP"))
  (setf (avatar-input-buffer avatar) (as:socket-data socket))
  (setf (as:socket-data socket) avatar)
  ;; NOTE: This write serves only to change the read callback.
  (as:write-socket-data socket "" :read-cb #'read-websocket-message))

(defun start-session (avatar location-id socket request)
  "Creates a new session for an avatar that is not already in the world."
  (sethash (avatar-account-id avatar) *avatars* avatar)
  (setup-session avatar socket request)
  (format-log :info "(~a) starting session for account-id ~d"
              (avatar-remote-ip avatar) (avatar-account-id avatar))
  (enter-world avatar)
  (enter-location avatar (find-location location-id) nil)
  (update-ui avatar))

(defun disconnect-session (avatar)
  "Disconnects the avatar from its socket but does not remove the avatar from
the world, as when the player navigates away from the game page."
  (with-slots (socket) avatar
    (when socket
      (format-log :info "(~a) disconnected account-id ~d"
                  (avatar-remote-ip avatar) (avatar-account-id avatar))
      (as:close-socket socket)
      (setf socket nil))))

(defun reconnect-session (avatar socket request)
  "Reconnects an avatar already in the world to a new socket."
  (setup-session avatar socket request)
  (format-log :info "(~a) reconnected account-id ~d"
              (avatar-remote-ip avatar) (avatar-account-id avatar))
  (send-queued-messages avatar)
  (update-ui avatar :for-location t))

(defun end-session (avatar)
  "Removes the avatar from the world, saves it, and closes the associated
websocket, if any."
  (format-log :info "(~a) ending session for account-id ~d"
              (avatar-remote-ip avatar)
              (avatar-account-id avatar))
  (let ((location-id (entity-label (entity-container avatar))))
    (exit-location avatar (entity-container avatar) nil :force t)
    (exit-world avatar)
    (save-avatar avatar location-id))
  (disconnect-session avatar)
  (remhash (avatar-account-id avatar) *avatars*))

;;;

(defun send-response (socket request status reason &key body headers cookies)
  "Sends an HTTP response and closes the socket once the send completes."
  (let ((response (make-http-response :status status :reason reason
                                      :body body :headers headers
                                      :version (http-request-version request))))
    (dolist (cookie cookies)
      (destructuring-bind (name . value) cookie
        (http-response-set-cookie response name value)))
    (as:write-socket-data socket (encode-http-response response)
                          :write-cb #'as:close-socket)
    (format-log :info "(~a) ~d ~a"
                (http-request-get-header request "X-Real-IP")
                status
                (http-request-path request))))

(defun get-authorization-for-request (request)
  "If `request' contains a basic HTTP authorization header, returns the
associated username and password."
  (let ((auth (http-request-get-header request "Authorization")))
    (when (and auth (equal (subseq auth 0 5) "Basic"))
      (cl-ppcre:split ":" (babel:octets-to-string
                           (cl-base64:base64-string-to-usb8-array (subseq auth 6)))
                      :limit 2))))

;;; HTTP request handlers

(defvar *request-handlers* (make-hash-table :test 'equal)
  "Mapping from URI path to handler function for all HTTP requests.")

(defun handle-auth-request (socket request)
  "Checks the session key in the request. If it is present and valid, returns a
200 response with the associated username in the body. Otherwise return a 401
response."
  (if-let ((username (car (validate-session-key (get-session-key request)))))
    (send-response socket request 200 "OK"
                   :body (format nil "{\"username\": ~s}~%" username))
    (send-response socket request 401 "Unauthorized")))

(setf (gethash "/auth" *request-handlers*) #'handle-auth-request)

(defun send-login-response (username account-id socket request)
  (let ((session-key (make-session-key username account-id)))
    (send-response socket request 200 "OK"
                   :cookies (list (cons *session-key-cookie* session-key))
                   :body (format nil "{\"username\": ~s}~%" username))
    session-key))

(defun handle-login-request (socket request)
  "If the request contains an Authorization header, extracts the username and
password and attempts to authenticate the user. On success, sends a 200 response
with the username in the body and a new session key in a cookie; otherwise,
sends a 401 response. If no Authorization header is present, sends a 400
response."
  (if-let ((auth (get-authorization-for-request request)))
    ;; Check the username and password.
    (destructuring-bind (username password) auth
      (if-let ((account-id (authenticate username password)))
        (send-login-response username account-id socket request)
        (send-response socket request 401 "Unauthorized"
                       :body (format nil "Invalid username or password.~%"))))
    ;; The request is invalid.
    (send-response socket request 400 "Bad Request")))

(setf (gethash "/login" *request-handlers*) #'handle-login-request)

(defun handle-create-request (socket request)
  "If the request contains an Authorization header, attempts to create a new
account. On success, sends a 200 response with the username in the body and a
new session key in a cookie. If the username or password is invalid or the
username is already in use, sends a 401 response with a description of the
problem in the body. If the Authorization header is missing, sends a 400
response."
  (if-let ((auth (get-authorization-for-request request)))
    ;; The request contains a proposed username and password.
    (destructuring-bind (username password) auth
      (if-let ((problem (or (validate-username username) (validate-password password))))
        (send-response socket request 401 "Unauthorized" :body (format nil "~s~%" problem))
        (if-let ((account-id (create-account username password
                                             (clone-entity (gethash :new-avatar-proto *config*))
                                             (gethash :new-avatar-location *config*))))
          (send-login-response username account-id socket request)
          (send-response socket request 401 "Unauthorized"
                         :body (format nil "Username already exists.~%")))))
    (send-response socket request 400 "Bad Request")))

(setf (gethash "/create" *request-handlers*) #'handle-create-request)

(defun handle-logout-request (socket request)
  (if-let ((account-id (cadr (validate-session-key (get-session-key request)))))
    (progn
      (when-let ((avatar (gethash account-id *avatars*)))
        (end-session avatar))
      (send-response socket request 200 "OK"
                     :cookies (list (cons *session-key-cookie* "invalid"))))
    (send-response socket request 400 "Bad Request")))

(setf (gethash "/logout" *request-handlers*) #'handle-logout-request)

(defun handle-session-request (socket request)
  "Validates the request headers to ensure this is a websocket handshake. If not,
sends a 400 response. Validates the session key cookie; if it is not valid,
returns a 401 response. On success, sends the server handshake and changes the
read-cb to start reading websocket messages. Finally, sends initial updates to
the client."
  (if-let ((client-key (websocket-validate-headers request)))
    (if-let ((session-key-values (validate-session-key (get-session-key request))))
      (bind (((username account-id nil) session-key-values))
        ;; Complete the websocket handshake. FIXME: why can this first write
        ;; not successfully change the read-cb?
        (as:write-socket-data socket (encode-http-response
                                      (websocket-make-accept-response client-key)))
        (if-let ((avatar (gethash account-id *avatars*)))
          (reconnect-session avatar socket request)
          (bind ((avatar location-id (load-avatar account-id)))
            (format-log :info "loaded avatar for user ~a (account-id ~d) at location ~s"
                        username account-id location-id)
            (start-session avatar location-id socket request))))
      (send-response socket request 401 "Unauthorized"))
    (send-response socket request 400 "Bad Request")))

(setf (gethash "/session" *request-handlers*) #'handle-session-request)

(defun handle-who-request (socket request)
  "Responds with a JSON-encoded summary of all connected players."
  (if (string= (http-request-method request) "HEAD")
      (send-response socket request 200 "OK"
                     :headers (list (cons "X-Num-Players" (hash-table-count *avatars*))))
      ;; FIXME: implement this.
      (send-response socket request 200 "OK" :body "[]")))

(setf (gethash "/who" *request-handlers*) #'handle-who-request)

;;; FIXME: only accept this request from localhost

(defun handle-stop-server-request (socket request)
  (send-response socket request 200 "OK")
  ;; Don't exit until after this request has been handled, otherwise the
  ;; response won't be sent.
  (with-delay (0)
    (as:exit-event-loop)))

(setf (gethash "/stopserver" *request-handlers*) #'handle-stop-server-request)

;;; Basic HTTP server: handle connections, read headers, and dispatch requests.

(defparameter *crlfcrlf* #(13 10 13 10))

(defun read-request-headers (socket data)
  "Appends `data' to the buffer for `socket' and interprets all buffered data as
HTTP request headers. If a complete set of headers is present, consumes them
from the buffer and dispatches the request based on the path specified in the
request."
  (let ((buffer (as:socket-data socket)))
    (buffer-push buffer data)
    (when-let ((end-of-headers (buffer-find buffer *crlfcrlf*)))
      (let ((request (parse-http-request (buffer-get buffer end-of-headers))))
        (buffer-consume buffer (+ end-of-headers (length *crlfcrlf*)))
        (if-let ((handler (gethash (http-request-path request) *request-handlers*)))
          (funcall handler socket request)
          (send-response socket request 404 "Not Found"))))))

(defun read-websocket-message (socket data)
  (let ((avatar (as:socket-data socket)))
    (buffer-push (avatar-input-buffer avatar) data)
    (handler-case
        (loop for message = (websocket-decode-message (avatar-input-buffer avatar))
              while message
              do
                 (destructuring-bind (opcode . payload) message
                   (cond
                     ((= opcode +websocket-op-text+) (process-input avatar payload))
                     ((= opcode +websocket-op-close+) (disconnect-session avatar))
                     (t
                      (format-log :warning "got unsupported websocket opcode ~d" opcode)
                      (end-session avatar)))))
      (websocket-error (err)
        (format-log :warning "read invalid message, terminating session: ~s"
                    (apply #'format nil
                           (simple-condition-format-control err)
                           (simple-condition-format-arguments err)))
        (end-session avatar)))))

(defun handle-connection (socket)
  "Associates an input buffer with a newly-connected socket."
  (setf (as:socket-data socket) (make-instance 'buffer)))

(defun start-world ()
  (format-log :info "starting ~d locations" (hash-table-count *locations*))
  (maphash-values #'enter-world *locations*))

(defun stop-world ()
  ;; Save all avatars and remove them from the world. Because end-session
  ;; modifies *avatars*, iterate over a copy of the table's values.
  (format-log :info "saving ~d avatars" (hash-table-count *avatars*))
  (mapcar #'end-session (hash-table-values *avatars*))
  ;; Remove each location from the world.
  (format-log :info "stopping ~d locations" (hash-table-count *locations*))
  (maphash-values #'exit-world *locations*))

(defun run-event-loop ()
  (as:with-event-loop ()
    (format-log :info "starting event loop")

    (open-database (gethash :root-directory *config*))
    (start-world)

    (as:add-event-loop-exit-callback
     (lambda ()
       (format-log :info "exiting event loop")
       (stop-world)
       (close-database)))

    ;; Start the server.
    (as:tcp-server
     (? *config* :server-address) (? *config* :server-port)
     #'read-request-headers
     :event-cb #'(lambda (event) (format t "ev: ~a~%" event))
     :connect-cb #'handle-connection)

    (format-log :info "listening at ~a:~d" (? *config* :server-address) (? *config* :server-port))

    ;; Arrange to exit the event loop on SIGINT.
    (as:signal-handler
     as:+sigint+
     (lambda (sig)
       (format-log :info "stopping with signal ~a" sig)
       (as:exit-event-loop)))))

(defun run-server ()
  (clrhash *avatars*)
  (run-event-loop))

(defun stop-server ()
  (as:trigger-notifier (as:make-notifier #'as:exit-event-loop)))
