(in-package :jade)

;;; An avatar is an entity that represents a player in the world. It defines
;;; additional slots used internally by the server.

(defclass avatar (entity)
  ;; TODO: document slots
  ((avatar-id :initarg :avatar-id :initform nil :accessor avatar-id)
   (account-id :initform nil :accessor avatar-account-id)
   ;; Slots related to the client connection.
   (socket :initform nil :accessor avatar-socket)
   (remote-ip :initform nil :accessor avatar-remote-ip)
   (input-buffer :initform nil :accessor avatar-input-buffer)
   (output-queue :initform (make-queue) :accessor avatar-output-queue)
   ;; Slots related to game mechanics.
   (tutorials-on :initform t :accessor tutorials-on)
   (tutorials-seen :initform (make-hash-table) :accessor tutorials-seen)
   (dirty-tutorials :initform nil :accessor dirty-tutorials)
   (active-quests :initform nil :accessor active-quests)
   (finished-quests :initform (make-hash-table) :accessor finished-quests)
   (dirty-quests :initform nil :accessor dirty-quests)
   (pending-offer :initform nil :accessor pending-offer)))

(sethash 'avatar *named-entities* (make-instance 'avatar :label 'avatar))

(defmethod print-object ((obj avatar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write (avatar-id obj) :stream stream)))

(defmethod clone-entity ((proto avatar) &rest attributes)
  (declare (ignore attributes))
  (let ((clone (call-next-method)))
    (setf (? clone :equipment) (make-hash-table :test #'eq))
    clone))

(defmethod transform-initval ((name (eql :race)) value)
  `(find-entity ',value))

;;; Encoding and decoding for avatar slots and attributes.

(defmethod encoded-slots ((entity avatar))
  '(tutorials-on active-quests))

(defmethod encode-value ((entity avatar) (name (eql :race)) value)
  (when value (entity-label value)))

(defmethod decode-value ((entity avatar) (name (eql :race)) data)
  (when (symbolp data) (find-entity data)))

(defmethod encode-value ((entity avatar) (name (eql :inventory)) value)
  (mapcar #'encode-entity value))

(defmethod decode-value ((entity avatar) (name (eql :inventory)) data)
  (mapcar #'decode-entity data))

(defmethod encode-value ((entity avatar) (name (eql :equipment)) value)
  (loop
    for slot being the hash-keys in value using (hash-value item)
    nconc (list slot (encode-entity item))))

(defmethod decode-value ((entity avatar) (name (eql :equipment)) data)
  (let ((equipment (make-hash-table)))
    (loop for (slot item-data) on data by #'cddr do
      (sethash slot equipment (decode-entity item-data)))
    equipment))

(defmethod encode-value ((entity avatar) (name (eql :skills)) value)
  (loop
    for skill being the hash-keys in value using (hash-value rank)
    nconc (list (skill-id skill) rank)))

(defmethod decode-value ((entity avatar) (name (eql :skills)) data)
  ;; TODO:
  nil)

;;;

(defmethod describe-brief ((avatar avatar) &key quantity (article :indefinite) capitalize)
  (declare (ignore quantity))
  (or (? avatar :name)
      (describe-brief (? avatar :race) :article article :capitalize capitalize)))

(defmethod match-subject (tokens (subject avatar))
  (best-match-quality (call-next-method)
                      (match-subject tokens (? subject :race))))

;;;

(defmacro for-avatars-in ((var location) &body body)
  `(dolist (,var (? ,location :contents))
     (when (typep ,var 'avatar)
       ,@body)))

;;;

(defun skill-rank (avatar skill)
  (? avatar :skills skill))

(defun xp-required-for-level (level)
  (+ (* 1000 level) (* 200 level (1- level))))

;;; The `:inventory' slot is a list of items carried by the avatar.

(defun encumbrance (avatar)
  (float (/ (apply #'+ (loop for item in (? avatar :inventory)
                             collect (* (? item :size) (? item :quantity))))
            (+ 100 (* 0.1 (or (? avatar :strength) 0))))))

(defun check-encumbrance (avatar)
  (when (>= (encumbrance avatar) 1.0)
    (show-notice avatar "You are encumbered.")))

;;; The `:equipment' attribute is a hash table mapping from equipment slots to
;;; equipped items.

(defparameter *equipment-slots*
  '(:main-hand :off-hand :tool
    :head :torso :back :hands :waist :legs :feet
    :ears :neck :left-wrist :right-wrist :left-finger :right-finger
    :backpack :belt-pouch)
  "Descriptions of slots in which an avatar can equip an item.")

(defun equipped-items (avatar &rest slots)
  (when-attributes (equipment) avatar
    (if slots
        (loop for slot in slots collect (gethash slot equipment))
        (hash-table-values equipment))))

;;; The `:race' attribute is an entity that defines some base attributes of the
;;; avatar.

#|
(defun change-race (avatar race)
  (setf (? avatar :race) race)
    (update-avatar avatar :race (describe-brief race :article nil))
    (show-notice avatar "You are now ~a!" (describe-brief race)))

(defun validate-name (name)
  "If `name' is a valid avatar name after stripping trailing punctuation, then
  return it after capitalizing it appropriately. Otherwise, return `nil'."
  (labels ((valid-char (ch) (or (alpha-char-p ch) (eql ch #\-) (eql ch #\'))))
    (let* ((end (position-if-not #'valid-char name))
           (bad (position-if #'valid-char name :start end)))
      (unless bad
        (let ((name (string-downcase (subseq name 0 end))))
          (setf (char name 0) (char-upcase (char name 0)))
          name)))))

(defun change-name (avatar name)
  ;; FIXME: check for naughty words?
  (when-let ((name (validate-name name)))
    (setf (name avatar) name)
    (update-avatar avatar :name name)
    (show-notice avatar "Your name is now ~a!" name)
    name))

;;;

(defun merge-traits (to from)
  "Given plist `from' of trait keys and values, add the values into hash table
  `to' under the matching keys."
  (loop for (trait value) on from by #'cddr do
    (setf (gethash trait to)
          (+ value (or (gethash trait to) 0))))
  to)

(defun calculate-traits (avatar)
  "Computes effective traits for `avatar' based on race, equipment, skills,
  and auras."
  ;; TODO: skills and auras
  (let ((traits (make-hash-table)))
    (with-slots (race equipment skills auras) avatar
      (when race
        (merge-traits traits (race-traits race)))
      (maphash-values #'(lambda (item)
                          (when item
                            (merge-traits traits (equipment-traits item))))
                      equipment))
    traits))
|#
