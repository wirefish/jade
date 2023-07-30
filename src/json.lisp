;;;; Functions to encode data as JSON.

(in-package :jade)

(defgeneric encode-json (value stream))

(defmethod encode-json ((value integer) stream)
  (write value :stream stream))

(defmethod encode-json ((value float) stream)
  (write value :stream stream))

(defconstant +json-char-substitutions+
  #h(#\\ "\\\\"
     #\" "\\\""
     #\Backspace "\\b"
     #\Page "\\f"
     #\Newline "\\n"
     #\Return "\\r"
     #\Tab "\\t"))

(defmethod encode-json ((value string) stream)
  (write-char #\" stream)
  (loop for ch across value do
       (let ((sub (gethash ch +json-char-substitutions+)))
         (if sub
             (write-string sub stream)
             (write-char ch stream))))
  (write-char #\" stream))

(defmethod encode-json ((value (eql t)) stream)
  (write-string "true" stream))

(defmethod encode-json ((value (eql :false)) stream)
  (write-string "false" stream))

(defmethod encode-json ((value symbol) stream)
  (write (string-downcase (substitute #\_ #\- (symbol-name value))) :stream stream))

(defmethod encode-json ((value null) stream)
  (write-string "null" stream))

(defmethod encode-json ((value list) stream)
  (write-char #\[ stream)
  (when value
    (encode-json (car value) stream)
    (dolist (item (cdr value))
      (write-char #\, stream)
      (encode-json item stream)))
  (write-char #\] stream))

(defmethod encode-json ((value hash-table) stream)
  (write-char #\{ stream)
  (loop for key being the hash-keys in value using (hash-value value) and index from 0 do
       (when (> index 0)
         (write-char #\, stream))
       (encode-json key stream)
       (write-char #\: stream)
       (encode-json value stream))
  (write-char #\} stream))
