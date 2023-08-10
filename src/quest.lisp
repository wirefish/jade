(in-package :jade)

#|

A quest moves through a sequence of phases as an avatar progresses through its
requirements, in the following order:

- :unavailable

- :available

- :offered

- "active" phases defined by the quest

- :finished

In the :offered and active phases, the avatar stores a list (phase . state) in
its active-quests map. If the avatar rejects an offered quest, or cancels the
quest while it is active, that entry is removed.

|#

(defclass quest ()
  ((label :initarg :label :initform nil :reader quest-label)
   (name :initarg :name :initform nil :reader quest-name)
   (summary :initarg :summary :initform nil :reader quest-summary)
   (level :initarg :level :initform 0 :reader quest-level)
   (xp-multiplier :initarg :xp-multiplier :initform 1.0 :reader quest-xp-multiplier)
   (required-quests :initarg :required-quests :initform nil :reader quest-required-quests)
   (phases :initarg :phases :initform nil :reader quest-phases)))

(defclass quest-phase ()
  ((label :initarg :label :initform nil :reader quest-phase-label)
   (summary :initarg :summary :initform nil :reader quest-phase-summary)
   (initial-state :initarg :initial-state :initform nil :reader quest-phase-initial-state)))

(defvar *quests* (make-hash-table :size 1000))

(defmacro defquest (label attributes &body phases)
  (with-gensyms (quest)
    `(let ((,quest (make-instance
                    'quest
                    :label ',label
                    ,@(loop for (key value) on attributes by #'cddr
                            nconc (list key
                                        (transform-initval key value))))))
       ;; TODO: phases
       (sethash ',label *quests* ,quest)
       (export ',label)
       ,quest)))

;;;

(defun begin-quest (avatar quest)
  "Enter the first phase defined by the quest."
  nil)

(defun cancel-quest (avatar quest)
  "Cancel the quest, removing any progress and any quest-related items."
  nil)

;;; Quest-related events.

(defgeneric accept-quest (avatar quest npc)
  (:documentation "Called when `avatar' accepts `quest' from `npc'."))

(defmethod accept-quest :around (avatar quest npc)
  (let ((observers (list avatar npc)))
    (notify-observers observers :before-accept-quest avatar quest npc)
    (call-next-method)
    (notify-observers observers :after-accept-quest avatar quest npc)))

(defmethod accept-quest (avatar quest npc)
  (begin-quest avatar (quest-id quest))
  (show-notice avatar "You have accepted the quest ~s." (quest-name quest))
  (show-map avatar))

(defgeneric offer-quest (npc quest avatar)
  (:documentation "Called when `npc' offers `quest' to `avatar'."))

(defmethod offer-quest :around (npc quest avatar)
  (let ((observers (list avatar npc)))
    (notify-observers observers :before-offer-quest avatar quest npc)
    (call-next-method)
    (notify-observers observers :after-offer-quest avatar quest npc)))

(defun accept-or-reject-quest (avatar quest npc accepted)
  "Called when `avatar' accepts or rejects an offer from `npc' to begin
`quest'."
  (if accepted
      (accept-quest avatar quest npc)
      (progn
        (show-notice avatar "You have rejected the quest ~s." (quest-name quest))
        (cancel-quest avatar quest)
        (show-map avatar))))

(defmethod offer-quest (npc quest avatar)
  (make-offer avatar #'accept-or-reject-quest avatar quest npc)
  (show-notice avatar
               "~a has offered you the level ~d quest ~s. Type `accept` to accept it."
               (describe-brief npc :capitalize t :article :definite)
               (quest-level quest)
               (quest-name quest)))
