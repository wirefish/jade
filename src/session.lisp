(in-package :jade)

;;; Session keys.

(defparameter *signing-key* (ironclad:random-data 32)
  "A secret key used when signing and validating session keys.")

(defparameter *signature-length* 32
  "The length of the array returned by the `signature' function.")

(defparameter *session-key-valid-seconds* 30
  "The maximum allowed time between the generation of a session key and its
first use to create a new session.")

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

(defstruct session
  socket
  remote-ip
  input-buffer
  (output-queue (make-queue)))

(defgeneric get-session (entity)
  (:documentation "Returns the session associated with `entity', if any.")
  (:method ((entity entity))
    nil)
  (:method ((entity avatar))
    (slot-value entity 'session)))
