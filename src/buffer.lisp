;;;; Defines a class that represents a buffer for a sequence of bytes. Bytes are
;;;; written to the tail of the buffer and read from the head of the buffer. The
;;;; buffer dynamically adjusts the size of its internal storage as needed.

(in-package :jade)

(defclass buffer ()
  ((head :type fixnum :initform 0)
   (tail :type fixnum :initform 0)
   (storage :type array
            :initform (make-array 64 :adjustable t :element-type '(unsigned-byte 8)))))

(defun buffer-length (buffer)
  "Returns the number of bytes in the buffer."
  (with-slots (head tail) buffer
    (- tail head)))

(defun buffer-data (buffer)
  "Returns a byte vector representing the contents of the buffer."
  (with-slots (head tail storage) buffer
    (make-array (- tail head) :element-type (array-element-type storage)
                :displaced-to storage :displaced-index-offset head)))

(defun buffer-push (buffer data)
  "Appends data to the tail of the buffer."
  (with-slots (head tail storage) buffer
    (let ((capacity (array-total-size storage)))
      (when (> (+ tail (length data)) capacity)
        ;; Extend the buffer storage to accommodate the new data.
        (adjust-array storage (max (* 2 capacity) (+ tail (length data)))))
      (replace storage data :start1 tail)
      (incf tail (length data)))))

(defun buffer-find (buffer seq &key (start 0))
  "Finds the first occurrence of `seq` not before offset `start` in the buffer,
and returns its position or nil if not found."
  (with-slots (head tail storage) buffer
    (let ((pos (search seq storage :start2 (+ head start) :end2 tail)))
      (and pos (- pos head)))))

(defun buffer-get (buffer count &key (start 0))
  "Returns a view of bytes in the buffer. This does not copy the bytes or remove
them from the buffer."
  (with-slots (head tail storage) buffer
    (let ((pos (+ head start)))
      (make-array count :element-type (array-element-type storage)
                  :displaced-to storage :displaced-index-offset pos))))

(defun buffer-consume (buffer count)
  "Consumes up to `count` bytes from the head of the buffer. Returns the actual
number of bytes consumed."
  (with-slots (head tail storage) buffer
    (setf count (min count (- tail head)))
    (incf head count)
    (when (= head tail)
      ;; Reset the buffer to its initial empty state.
      (setf head 0 tail 0))
    count))
