(in-package :jade)

;;; An avatar has an associated race that defines some baseline properties for
;;; the avatar. An avatar's race can change over time.

(defvar *races* (make-hash-table))

(defstruct race
  name brief icon traits)

(defmacro defrace (name &body body)
  `(progn
     (setf (gethash ',name *races*)
           (make-race :name ',name ,@body))  ;; FIXME: transforms
     (export ',name)))

;;; An avatar is an entity that represents a player in the world. It defines
;;; additional slots used internally by the server.

(defclass avatar (entity)
  ((avatar-id :initarg :avatar-id :initform nil :reader avatar-id)
   (session :initform nil)
   ;;
   (tutorials-on :initform t)
   (tutorials-seen :initform (make-hash-table))
   (dirty-tutorials :initform nil)  ; list of tutorials for which state has changed since save
   ;;
   (active-quests :initform nil)  ; alist of (quest-name . state)
   (finished-quests :initform (make-hash-table))  ; quest-name -> completion time
   (dirty-quests :initform nil)  ; names of quests for which state has changed since save
   ;;
   (pending-offer :initform nil)))  ; function to call if player enters "accept"

(export (defparameter avatar (make-instance 'avatar :label 'avatar)))

(defmethod print-object ((obj avatar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write (avatar-id obj) :stream stream)))

;;; Encoding and decoding for avatars slots and attributes.

(defmethod encoded-slots ((entity avatar))
  '(tutorials-on active-quests))

(defmethod encode-value ((entity avatar) (name (eql :race)) value)
  (when value (race-name value)))

(defmethod decode-value ((entity avatar) (name (eql :race)) data)
  (when (symbolp data) (gethash data *races*)))

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

(defun skill-rank (avatar skill)
  (? avatar :skills skill))

(defun xp-required-for-level (level)
  (+ (* 1000 level) (* 200 level (1- level))))

;;;

#|
(defun change-race (avatar race-id)
  (when-let ((race (find-race race-id)))
    (setf (race avatar) race)
    (update-avatar avatar :race (describe-brief race :article nil))
    (show-notice avatar "You are now ~a!" (describe-brief race))))

(defun change-gender (avatar gender)
  (setf (gender avatar) gender)
  (show-notice avatar "You are now a ~(~a~)!" gender))

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