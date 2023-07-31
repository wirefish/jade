(in-package :jade)

(defun send-response (socket request status reason &key body headers cookies)
  "Sends an HTTP response and closes the socket once the send completes."
  (let ((response (make-http-response :status status :reason reason
                                      :body body :headers headers
                                      :version (http-request-version request))))
    (dolist (cookie cookies)
      (destructuring-bind (name . value) cookie
        (format t "~s ~s~%" name value)
        (http-response-set-cookie response name value)))
    (as:write-socket-data socket (encode-http-response response)
                          :write-cb #'as:close-socket)))

;; Name of the cookie used to store the session key.
(defparameter *session-key-cookie* "jade-session-key")

(defun get-session-for-request (request)
  (let* ((cookies (http-request-get-cookies request))
         (session-key (cdr (assoc *session-key-cookie* cookies :test #'string-equal))))
    (gethash session-key *sessions*)))

(defun get-authorization-for-request (request)
  (let ((auth (http-request-get-header request "Authorization")))
    (when (and auth (equal (subseq auth 0 5) "Basic"))
      (cl-ppcre:split ":" (babel:octets-to-string
                           (cl-base64:base64-string-to-usb8-array (subseq auth 6)))
                      :limit 2))))

;;; HTTP request handlers

;; Mapping from URI path to handler function for all HTTP requests.
(defvar *request-handlers* (make-hash-table :test 'equal))

(defun handle-auth-request (socket request)
  "Checks the session key in the request. If it is present and valid, returns a
  200 response with the associated username in the body. Otherwise return a 401
  response."
  (let ((session (get-session-for-request request)))
    (if session
        ;; The client's session key is still valid; return success with the
        ;; username in the body.
        (send-response socket request 200 "OK"
                       :body (format nil "{\"username\": ~s}" (session-username session)))
        ;; The client needs to login.
        (send-response socket request 401 "Unauthorized"))))

(setf (gethash "/auth" *request-handlers*) #'handle-auth-request)

(defun create-session (socket request account-id username)
  (let* ((session (make-session :account-id account-id :username username)))
    (setf (gethash (session-key session) *sessions*) session)
    (format-log :info "created session ~a for user ~a" (session-key session) username)
    (send-response socket request 200 "OK"
                   :cookies (list (cons *session-key-cookie* (session-key session)))
                   :body (format nil "{\"username\": ~s}" username))))

(defun handle-login-request (socket request)
  "If the request contains an Authorization header, extracts the username and
  password and attempts to create a new session for the user. On success, sends
  a 200 response with the username in the body; otherwise, sends a 401 response.
  If no Authorization header is present, sends a 400 response."
  (let ((auth (get-authorization-for-request request)))
    (if auth
        ;; Check the username and password.
        (destructuring-bind (username password) auth
          (if-let ((account-id (authenticate username password)))
            ;; Create a new session.
            (create-session socket request account-id username)
            ;; Authentication failed.
            (send-response socket request 401 "Unauthorized"
                           :body "Invalid username or password.")))
        ;; The request is invalid.
        (send-response socket request 400 "Bad Request"))))

(setf (gethash "/login" *request-handlers*) #'handle-login-request)

(defun create-avatar ()
  (clone-entity (gethash :new-avatar-proto *config*)))

(defun handle-create-request (socket request)
  "If the request contains an Authorization header, attempts to create a new
  account. On success, sends a 200 response with the username in the body. If
  the username or password is invalid or the username is already in use, sends a
  401 response with a description of the problem in the body. If the
  Authorization header is missing, sends a 400 response."
  (let ((auth (get-authorization-for-request request)))
    (if auth
        ;; The request contains a proposed username and password.
        (destructuring-bind (username password) auth
          (if-let ((problem (or (validate-username username) (validate-password password))))
            ;; The username and/or password is structurally invalid.
            (send-response socket request 401 "Unauthorized" :body problem)
            (if-let ((account-id (create-account username password (create-avatar)
                                                 (gethash :new-avatar-location *config*))))
              ;; The account was created. Create a new session.
              (create-session socket request account-id username)
              ;; The account could not be created, generally because the
              ;; account name is already in use.
              (send-response socket request 401 "Unauthorized"
                             :body "Username already exists."))))
        ;; The request is invalid.
        (send-response socket request 400 "Bad Request"))))

(setf (gethash "/create" *request-handlers*) #'handle-create-request)

(defun handle-logout-request (socket request)
  (let ((session (get-session-for-request request)))
    (when session
      ;; TODO: close the session.
      (remhash (session-key session) *sessions*))
    (send-response socket request 200 "OK")))

(setf (gethash "/logout" *request-handlers*) #'handle-logout-request)

(defun start-session (socket session)
  "Called when the user creates a websocket connection associated with a
  specific session. This can occur multiple times for a single session."
  ;; TODO: load avatar if not already present in session. Send intro text, map
  ;; update, room description and contents, etc.
  (format-log :info "starting session ~a for user ~a"
              (session-key session) (session-username session))
  ;; NOTE: This write serves only to change the read callback.
  (as:write-socket-data socket "" :read-cb #'read-websocket-message)
  (send-client-command session "setAvatar" 12345)  ; FIXME:
  (connect-session session socket))

(defun handle-session-request (socket request)
  "Validates the request headers to ensure this is a websocket handshake. If
  not, return 400. Validate the session key cookie; if it is not valid return
  401. On success, send the server handshake and change the read-cb to start
  reading websocket messages. Also write initial updates to the client."
  (let ((client-key (websocket-validate-headers request)))
    (if (null client-key)
        ;; The websocket handshake is not valid.
        (send-response socket request 400 "Bad Request")
        ;; Look for the session key.
        (let* ((cookies (http-request-get-cookies request))
               (session-key (cdr (assoc *session-key-cookie* cookies :test #'string-equal)))
               (session (gethash session-key *sessions*)))
          (if session
              ;; Accept the request, associate this socket with the session, and
              ;; load the avatar into the world if this is a new session.
              (progn
                (setf (session-input-buffer session) (as:socket-data socket))
                (setf (as:socket-data socket) session)
                ;; FIXME: why can this first write not successfully change the read-cb?
                (as:write-socket-data socket (encode-http-response
                                              (websocket-make-accept-response client-key)))
                (when (null (session-avatar session))
                  ;; This is the first connection to this section. Load the
                  ;; avatar object and place it into the world.
                  (multiple-value-bind (avatar location-id)
                      (load-avatar (session-account-id session))
                    (format-log :info "loaded avatar ~s at location ~a"
                                avatar location-id)
                    (setf (session-avatar session) avatar)
                    (setf (avatar-session avatar) session)
                    (enter-world avatar)
                    (enter-location avatar (symbol-value location-id) nil)))
                (start-session socket session))
              ;; The session key is missing or invalid; the player needs to
              ;; authenticate.
              (send-response socket request 401 "Unauthorized"))))))

(setf (gethash "/session" *request-handlers*) #'handle-session-request)

(defun handle-who-request (socket request)
  "Responds with a JSON-encoded summary of all connected players."
  ;; FIXME: implement this.
  (send-response socket request 200 "OK" :body "[]"))

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
  "Appends `data` to the buffer for `socket` and interprets all buffered data as
  HTTP request headers. If a complete set of headers is present, consumes them
  from the buffer and dispatches the request based on the path specified in the
  request."
  (let ((buffer (as:socket-data socket)))
    (buffer-push buffer data)
    (let ((end-of-headers (buffer-find buffer *crlfcrlf*)))
      (when end-of-headers
        (let ((request (parse-http-request (buffer-get buffer end-of-headers))))
          (format-log :info "(~a) received request ~a"
                  (http-request-get-header request "X-Real-IP")
                  (http-request-path request))
          (buffer-consume buffer (+ end-of-headers (length *crlfcrlf*)))
          (let ((handler (gethash (http-request-path request) *request-handlers*)))
            (if handler
                (funcall handler socket request)
                (send-response socket request 404 "Not Found"))))))))

(defun close-session (session)
  ;; TODO: save the avatar
  (format-log :info "closing session ~s" (session-key session))
  (let ((avatar (session-avatar session)))
    (exit-location avatar (location avatar) nil :force t)
    (exit-world avatar))
  (as:close-socket (session-socket session))
  (remhash (session-key session) *sessions*))

(defun read-websocket-message (socket data)
  (let ((session (as:socket-data socket)))
    (buffer-push (session-input-buffer session) data)
    (handler-case
        (loop for message = (websocket-decode-message (session-input-buffer session))
              while message
              do
                 (destructuring-bind (opcode . payload) message
                   (cond
                     ((= opcode +websocket-op-text+)
                      (process-input (session-avatar session) payload))
                     (t
                      (format-log :warn "got unsupported websocket opcode ~d" opcode)
                      (close-session session)))))
      (websocket-error (err)
        (format-log :warn "read invalid message, terminating session: ~s"
                    (apply #'format nil
                           (simple-condition-format-control err)
                           (simple-condition-format-arguments err)))
        (close-session session)))))

(defun handle-connection (socket)
  "Associates an input buffer with a newly-connected socket."
  (setf (as:socket-data socket) (make-instance 'buffer)))

(defun start-world ()
  (format-log :info "starting ~d locations" (hash-table-count *locations*))
  (maphash-values #'enter-world *locations*))

(defun stop-world ()
  ;; Save all avatars and remove them from the world.
  (format-log :info "closing ~d sessions" (hash-table-count *sessions*))
  (maphash-values #'(lambda (session)
                      (with-slots (account-id (av avatar)) session
                        (let ((location (location av)))
                          (exit-location av location nil)
                          (exit-world av)
                          (save-avatar av (proto-name location)))))
                  *sessions*)
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
     (gethash :server-address *config*) (gethash :server-port *config*)
     #'read-request-headers
     :event-cb #'(lambda (event) (format t "ev: ~a~%" event))
     :connect-cb #'handle-connection)

    ;; Arrange to exit the event loop on SIGINT.
    (as:signal-handler
     as:+sigint+
     (lambda (sig)
       (format-log :info "stopping with signal ~a" sig)
       (as:exit-event-loop)))))

(defun run-server ()
  (clrhash *sessions*)
  (run-event-loop))

(defun stop-server ()
  (as:trigger-notifier (as:make-notifier #'as:exit-event-loop)))
