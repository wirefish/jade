(in-package :jade)

;;; An avatar is an entity that represents a player in the world. It defines
;;; additional slots used internally by the server.

(defclass avatar (combatant)
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
   (pending-offer :initform nil :accessor pending-offer)
   (karma :initform 0 :accessor karma)
   (skills :initform (make-hash-table) :accessor skills)
   (is-dead :initform nil :accessor is-dead)
   (client-state :initform (make-hash-table :test #'equalp) :accessor client-state)))

(defentity avatar (combatant &class avatar)
  (:energy 100
   :max-energy 100
   :xp 0
   :attitude :friendly))

(defmethod print-object ((obj avatar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write (avatar-id obj) :stream stream)))

(defmethod clone-entity ((proto avatar) &rest attributes)
  (declare (ignore attributes))
  (let ((clone (call-next-method)))
    (setf (? clone :equipment) (make-hash-table :test #'eq))
    clone))

(defmethod transform-initval (class (name (eql :race)) value)
  (symbol-value value))

(defmethod entity-size ((avatar avatar))
  (or (? avatar :size) (? avatar :race :size) +medium+))

;;; Encoding and decoding for avatar slots and attributes.

(defmethod encoded-slots ((entity avatar))
  '(tutorials-on active-quests karma skills))

(defmethod encode-value ((entity avatar) (name (eql 'skills)) value)
  (hash-table-plist value))

(defmethod decode-value ((entity avatar) (name (eql 'skills)) value)
  (plist-hash-table value))

(defmethod encode-value ((entity avatar) (name (eql :race)) value)
  (and value (entity-label value)))

(defmethod decode-value ((entity avatar) (name (eql :race)) data)
  (symbol-value-or-nil data))

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

;;;

(defparameter *spirit-noun* (parse-noun "spirit[s] of ~a"))

(defmethod describe-brief ((avatar avatar) &key quantity (article :indefinite) capitalize)
  (declare (ignore quantity))
  (if (is-dead avatar)
      (format nil (format-noun *spirit-noun* :article article :capitalize capitalize)
              (or (? avatar :name) (describe-brief (? avatar :race))))
      (or (? avatar :name)
          (describe-brief (? avatar :race) :article article :capitalize capitalize))))

(defmethod describe-full ((avatar avatar))
  (if (is-dead avatar)
      (format nil "The ghostly spirit of ~a hovers nearby."
              (or (? avatar :name) (describe-brief (? avatar :race))))
      (call-next-method)))

(defmethod get-icon ((avatar avatar))
  (if (is-dead avatar)
      'ghost
      (or (call-next-method) (? avatar :race :icon))))

(defmethod match-subject (tokens (subject avatar))
  (best-match-quality (call-next-method)
                      (match-subject tokens (? subject :race))))

(defmethod can-see ((avatar avatar) (item item))
  ;; FIXME: narrow to specific quest phase?
  (and (call-next-method)
       (let ((quest (? item :quest)))
         (or (null quest)
             (assoc quest (active-quests avatar))))))

;;;

(defmacro for-avatars-in ((var location) &body body)
  `(dolist (,var (? ,location :contents))
     (when (typep ,var 'avatar)
       ,@body)))

(defmacro for-avatars-near ((var origin radius) &body body)
  (with-gensyms (dx dy dz loc)
    `(loop for (,dx ,dy ,dz ,loc) in (walk-map ,origin ,radius :up-down t :cross-domains t)
           do (for-avatars-in (,var ,loc)
                ,@body))))

;;; Regeneration.

(defmethod regenerate ((avatar avatar))
  (unless (is-dead avatar)
    (call-next-method)
    (with-attributes (energy max-energy) avatar
      (setf energy (min max-energy (+ energy 10)))
      (update-avatar avatar :health :energy))))

;;; Experience.

(defun xp-required-for-level (level)
  (/ (* 1000 level (1- level)) 2))

(defun xp-granted-by-kill (level)
  (ceiling (/ (xp-required-for-level (1+ level)) 20 level)))

(defun xp-granted-by-quest (level)
  (* 5 (xp-granted-by-kill level)))

(defun gain-level (avatar)
  (show-notice avatar "You are now level ~d!"
               (incf (? avatar :level)))
  (setf (? avatar :max-health) (max-health avatar))
  (incf (karma avatar) 10)
  (update-avatar avatar :level :xp :max-xp :max-health)
  (update-skills avatar))

(defun gain-xp (avatar xp)
  (show avatar "You gain ~d experience." xp)
  (let ((xp (incf (? avatar :xp) xp))
        (xp-needed (xp-required-for-level (1+ (? avatar :level)))))
    (if (>= xp xp-needed)
        (progn
          (decf (? avatar :xp) xp-needed)
          (gain-level avatar))
        (update-avatar avatar :xp))))

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
  (with-attributes (equipment) avatar
    (loop for slot in (or slots *equipment-slots*)
          collect (gethash slot equipment))))

;;; The `:race' attribute is an entity that defines some base attributes of the
;;; avatar.

(defun change-race (avatar race-label)
  (when-let ((race (symbol-value-as 'entity race-label)))
    (setf (? avatar :race) race)
    (update-avatar avatar :race)
    (show-notice avatar "You are now ~a!" (describe-brief race))))

(defun run-lengths (s)
  "Returns a list of the lengths of successive substrings of `s' that consist of
the same character as compared by char-equal."
  (let ((current #\Nul) count runs)
    (loop for c across s
          do (if (char-equal c current)
                 (incf count)
                 (progn
                   (when count
                     (push count runs))
                   (setf count 1 current c))))
    (when count
        (push count runs))
    (nreverse runs)))

(defun validate-name (name)
  "If `name' is a valid avatar name after stripping trailing punctuation, then
  return it after capitalizing it appropriately. Otherwise, return `nil'."
  (labels ((valid-char (c) (or (alpha-char-p c) (eql c #\-) (eql c #\'))))
    (let ((name (string-right-trim '(#\! #\? #\.) name)))
      (when (and (>= (length name) 3)
                 (every #'valid-char name)
                 (alpha-char-p (char name 0))
                 (alpha-char-p (char name (1- (length name))))
                 (<= (count #\- name) 2)
                 (null (search "--" name))
                 (<= (count #\' name) 1)
                 (< (apply #'max (run-lengths name)) 3))
        (format nil "~@(~a~)" name)))))

(defun change-name (avatar name)
  ;; FIXME: check for naughty words?
  (when-let ((name (validate-name name)))
    (setf (? avatar :name) name)
    (update-avatar avatar :name)
    (show-notice avatar "Your name is now ~a!" name)
    name))

;;;

(defun reset-client-state (avatar)
  (clrhash (client-state avatar)))

(defun clear-client-state (avatar &rest keys)
  (with-slots (client-state) avatar
    (loop for key in keys do (remhash key client-state))))

(defun update-client-state (avatar key value &key (test #'eql))
  "Returns `value' if it is different than the avatar's current client state for
`key', or nil otherwise."
  (with-slots (client-state) avatar
      (unless (funcall test value (gethash key client-state))
        (sethash key client-state value)
        value)))

;;; Avatar-specific implementations of combat-related generic functions.

(defmethod merge-traits ((avatar avatar) cache)
  (maphash-values (lambda (item)
                    (merge-traits item cache))
                  (? avatar :equipment))
  (when-let ((race (? avatar :race)))
    (merge-traits race cache))
  (sethash :armor cache (armor-defense avatar))
  (call-next-method))

(defmethod select-attack ((actor avatar) target)
  (or (? actor :equipment :main-hand)
      (call-next-method)))

(defmethod describe-attack ((avatar avatar) actor target attack damage crit)
  (with-attributes (brief attack-verb critical-verb damage-type) attack
    (let ((verb (or (when crit critical-verb) attack-verb (attack-verb damage-type))))
      (if (eq avatar target)
          (update-avatar avatar :health)
          (update-neighbor avatar target :health (? target :health)))
      (cond
        ((eq avatar actor)
         (format nil "You ~a ~a ~@[with ~a ~]for ~d damage!"
                 (or (and verb (verb-plural verb)) "attack")
                 (describe-brief target)
                 (and brief (describe-brief attack))
                 damage))
        ((eq avatar target)
         (format nil "~a ~a you ~@[with ~a ~]for ~d damage!"
                 (describe-brief actor :capitalize t)
                 (or (and verb (verb-singular verb)) "attacks")
                 (and brief (describe-brief attack))
                 damage))
        (t
         (format nil "~a ~a ~a ~@[with ~a ~]for ~d damage!"
                 (describe-brief actor :capitalize t)
                 (or (and verb (verb-singular verb)) "attacks")
                 (describe-brief target)
                 (and brief (describe-brief attack))
                 damage))))))

(defmethod describe-death ((avatar avatar) actor target)
  (cond
    ((eq avatar actor)
     (format nil "You kill ~a!" (describe-brief target)))
    ((eq avatar target)
     (format nil "~a kills you!" (describe-brief actor :capitalize t)))
    (t
     (format nil "~a kills ~a!"
             (describe-brief actor :capitalize t)
             (describe-brief target)))))

(defmethod kill :after ((avatar avatar) (target combatant))
  (gain-xp avatar (xp-granted-by-kill (? target :level))))

(defmethod die ((avatar avatar))
  (setf (is-dead avatar) t)
  (reject-pending-offer avatar)
  (update-avatar avatar :icon)
  (for-avatars-in (a (location avatar))
    (unless (eq a avatar)
      (update-neighbor a avatar :icon :brief))))

(defmethod spawn-corpse ((avatar avatar) attackers)
  (when-let ((corpse (call-next-method)))
    (setf (corpse-owners corpse) (list avatar))
    ;; TODO: drop items?
    corpse))
