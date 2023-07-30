;;;; A basic implementation of encoding and decoding websocket messages.

(in-package :jade)

;; Bits in the first byte of a message.
(defconstant +websocket-fin+ #x80)  ; 1 if last fragment of message
(defconstant +websocket-op-bits+ #x0f)  ; mask for opcode, one of the following
(defconstant +websocket-op-text+ #x01)
(defconstant +websocket-op-binary+ #x02)
(defconstant +websocket-op-close+ #x08)
(defconstant +websocket-op-ping+ #x09)
(defconstant +websocket-op-pong+ #x0a)

;; Bits in the second byte of a message.
(defconstant +websocket-masked+ #x80)  ; 1 if client->server, else 0
(defconstant +websocket-length-bits+ #x7f)  ; length if 0-125
(defconstant +websocket-length-16+ #x7e)  ; length in next 16 bits, big endian
(defconstant +websocket-length-64+ #x7f)  ; length in next 64 bits, big endian

(define-condition websocket-error (error)
  ((reason :initarg :reason :reader reason)))

(defun websocket-mask-payload (masking-key data &key (start 0) end)
  "Returns the result of applying `masking-key' to `data'."
  (let ((payload (make-array (- end start) :element-type '(unsigned-byte 8))))
    (dotimes (i (length payload))
      (setf (elt payload i) (logxor (elt data (+ start i))
                                    (elt masking-key (mod i (length masking-key))))))
    payload))

(defun websocket-decode-message (buffer &key (require-masked t))
  "Decodes a websocket message. Signals `websocket-error' if the message is
invalid. Returns nil if `buffer' does not contain a complete message. If
`buffer' contains a complete, valid message, consumes the message from `buffer'
and returns the pair (opcode . payload). If opcode is `+websocket-op-text+', the
payload is decoded as UTF-8 before being returned."
  (let ((data (buffer-data buffer)))
    (when (>= (length data) 2)
      (let ((fin (/= (logand (elt data 0) +websocket-fin+) 0))
            (opcode (logand (elt data 0) +websocket-op-bits+))
            (masked (/= (logand (elt data 1) +websocket-masked+) 0))
            (length (logand (elt data 1) +websocket-length-bits+))
            (masking-key nil)
            (pos 2))
        (when (not fin)
          (error 'websocket-error :reason "Multi-frame messages are not supported."))
        (when (and require-masked (not masked))
          (error 'websocket-error :reason "Message must be masked."))
        (when (= length +websocket-length-64+)
          (error 'websocket-error :reason "Messages with 64-bit length are not supported."))
        (when (= length +websocket-length-16+)
          (if (>= (length data) (+ pos 2))
              ;; The next two bytes contain the payload length.
              (progn
                (setf length (ironclad:ub16ref/be data pos))
                (incf pos 2))
              ;; The message is incomplete.
              (return-from websocket-decode-message)))
        (when masked
          (if (>= (length data) (+ pos 4))
              ;; The next four bytes contain the masking key.
              (progn
                (setf masking-key (subseq data pos (+ pos 4)))
                (incf pos 4))
              ;; The message is incomplete.
              (return-from websocket-decode-message)))
        (when (>= (length data) (+ pos length))
          ;; The entire payload is present.
          (let ((payload (if masked
                             (websocket-mask-payload masking-key data
                                                     :start pos :end (+ pos length))
                             (subseq data pos (+ pos length)))))
            (buffer-consume buffer (+ pos length))
            (cons opcode (if (= opcode +websocket-op-text+)
                             (babel:octets-to-string payload :encoding :utf-8)
                             payload))))))))

(defun websocket-encode-message (payload &key (opcode +websocket-op-text+))
  "Returns a byte vector that represents an encoded server->client (i.e.
unmasked) websocket message. If `opcode` is `+websocket-op-text+`, then
`payload` is assumed to be a string and is encoded as UTF-8. Otherwise `payload`
assumed to be a byte vector and is used as-is. Signals `websocket-error` if the
message cannot be encoded."
  (let* ((encoded-payload (if (= opcode +websocket-op-text+)
                              (babel:string-to-octets payload :encoding :utf-8)
                              payload))
         (payload-length (length encoded-payload))
         (header nil))
    (cond
      ((> payload-length #xffff)
       (error 'websocket-error :reason "Messages with 64-bit length are not supported."))
      ((> payload-length 125)
       ;; Use the extended 16-bit payload length.
       (setf header (make-array 4 :element-type '(unsigned-byte 8)))
       (setf (elt header 0) (logior +websocket-fin+ opcode))
       (setf (elt header 1) +websocket-length-16+)
       (setf (ironclad:ub16ref/be header 2) payload-length))
      (t
       ;; Use the 7-bit payload length.
       (setf header (make-array 2 :element-type '(unsigned-byte 8)))
       (setf (elt header 0) (logior +websocket-fin+ opcode))
       (setf (elt header 1) payload-length)))
    (concatenate '(vector (unsigned-byte 8)) header encoded-payload)))

(defun websocket-validate-headers (request)
  "Checks that the given HTTP request contains the required websocket headers.
If so, returns the value of the Sec-WebSocket-Key header. Otherwise, returns
nil."
  (let ((client-key (http-request-get-header request "Sec-WebSocket-Key")))
    (when (and client-key
               (= (length (cl-base64:base64-string-to-usb8-array client-key)) 16)
               (string-equal (http-request-method request) "GET")
               (string-equal (http-request-version request) "HTTP/1.1")
               (string-equal (http-request-get-header request "Upgrade") "websocket")
               (string-equal (http-request-get-header request "Connection") "upgrade")
               (string-equal (http-request-get-header request "Sec-WebSocket-Version") "13"))
      client-key)))

(defparameter *websocket-magic* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun websocket-make-accept-response (client-key)
  "Returns an `http-response` indicating that the server is accepting the
client's request to initiate a websocket connection, where `client-key` is the
value of the Sec-WebSocket-Key header in the client request."
  (let ((server-key (cl-base64:usb8-array-to-base64-string
                     (ironclad:digest-sequence
                      :sha1
                      (babel:string-to-octets
                       (concatenate 'string client-key *websocket-magic*))))))
    (make-http-response
     :status 101 :reason "Switching Protocols"
     :headers `(("Upgrade" . "websocket")
                ("Connection" . "upgrade")
                ("Sec-WebSocket-Accept" . ,server-key)))))
