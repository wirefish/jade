(in-package :jade)

#|

A quest moves through a sequence of phases as an avatar progresses through its
requirements, in the following order:

- :unavailable when the avatar has not finished the quest and does not meet the
  requirements to accept it;

- :available when the avatar can accept the quest;

- :offered when the quest has been offered to the avatar, to be accepted or
  rejected;

- "active" phases defined by the quest while the avatar is performing the quest;

- :finished when the avatar has completed the quest.

In the :offered phase, the avatar stores (:offered nil) in its active-quests
table. In active phases, the list (phase-index state) is stored in the table.
The entry is updated as the avatar progresses through the quest. It is removed
when the avatar rejects an offered quest, cancels an active quest, or finishes a
quest.

In the :finished phase, the time of completion is stored in the avatar's
finished-quests table.

The state associated with a quest phase can take one of three forms:

- nil, in which case the phase is complete whenever it is advanced.

- a number, in which case the phase is complete when advancing causes its value
  to become >= 1.

- an alist with numeric values, in which case the phase is complete when
  advancing causes all values to become >= 1.

|#

(defclass quest ()
  ((label :initarg :label :initform nil :reader quest-label)
   (name :initarg :name :initform nil :reader quest-name)
   (summary :initarg :summary :initform nil :reader quest-summary)
   (level :initarg :level :initform 0 :reader quest-level
          :documentation "The minimum level required to accept the quest.
          Determines the base XP reward.")
   (xp-multiplier :initarg :xp-multiplier :initform 1.0 :reader quest-xp-multiplier
                  :documentation "Scales the XP reward of the quest.")
   (required-quests :initarg :required-quests :initform nil :reader quest-required-quests
                    :documentation "labels of quests that must be completed
                    before this quest can be accepted.")
   (can-accept :initarg :can-accept :initform nil :reader quest-can-accept
               :documentation "A unary function that is passed an avatar and can
               do additional checks to determine if the avatar can accept the
               quest.")
   (phases :initarg :phases :initform nil :reader quest-phases)))

(defclass quest-phase ()
  ((label :initarg :label :initform nil :reader quest-phase-label)
   (summary :initarg :summary :initform nil :reader quest-phase-summary)
   (initial-state :initarg :initial-state :initform nil :reader quest-phase-initial-state
                  :documentation "The state stored upon entering this phase.")
   (complete :initarg :complete :initform nil :reader quest-phase-complete
             :documentation "A unary function that is passed the current state
             and returns t if the phase has been completed.")))

(defvar *quests* (make-hash-table :size 1000))

(defun find-quest (label)
  (gethash label *quests*))

(defmacro defquest (label attributes &body phases)
  (with-gensyms (quest)
    `(let ((,quest (make-instance
                    'quest
                    :label ',label
                    ,@(loop for (key value) on attributes by #'cddr
                            nconc (list key (transform-initval key value)))
                    :phases
                    (list
                     ,@(loop for (label . attributes) in phases
                             collect `(make-instance
                                       'quest-phase
                                       :label ',label
                                       ,@(loop for (key value) on attributes by #'cddr
                                               nconc (list key
                                                           (transform-initval key value)))))))))
       (sethash ',label *quests* ,quest)
       (export ',label)
       ,quest)))

(defmethod transform-initval ((name (eql :complete)) value)
  value)

(defmethod transform-initval ((name (eql :required-quests)) value)
  `(quote ,value))

(defmethod transform-initval ((name (eql :offers-quests)) value)
  `(quote ,value))

;;;

(defun begin-quest (avatar quest)
  "Enter the first phase defined by the quest."
  nil)

(defun cancel-quest (avatar quest)
  "Cancel the quest, removing any progress and any quest-related items."
  ;; FIXME: items
  (remhash (quest-label quest) (active-quests avatar)))

(defun can-accept-quest (avatar quest)
  (with-slots (label level required-quests can-accept) quest
    (and (not (gethash label (finished-quests avatar)))
         (>= (? avatar :level) level)
         (every #`(gethash % (finished-quests avatar)) required-quests)
         (or (null can-accept) (funcall can-accept avatar)))))

(defun quest-phase (avatar quest-label)
  (if (gethash quest-label (finished-quests avatar))
      :finished
      (if-let ((state (gethash quest-label (active-quests avatar))))
        (first state)
        (if (can-accept-quest avatar (find-quest quest-label))
            :available
            :unavailable))))

(defun active-quest-state (avatar quest-label)
  (gethash quest-label (active-quests avatar)))

(defun set-active-quest-state (avatar quest-label phase &optional state)
  (sethash quest-label (active-quests avatar) (list phase state)))

(defun advance-quest-state (state &optional arg1 arg2)
  "Returns two values: the new state, and t if the new state indicates the phase
is complete."
  (typecase state
    (null (values nil t))
    (number
     (let ((n (+ arg1 state)))
       (values n (>= n 1))))
    (list
     (let ((entry (assoc arg1 state)))
       (incf (cdr entry) arg2)
       (values state (every #`(>= (cdr %) 1) state))))))

(defun advance-quest (avatar quest &optional arg1 arg2)
  "Updates an avatar's state for an active quest and, if the current phase becomes
complete, advances to the next phase. Returns the index of the new phase, or
:finished if all phases have been completed."
  (with-slots (label phases) quest
    (bind (((phase &optional state) (active-quest-state avatar label))
           (new-state phase-complete (advance-quest-state state arg1 arg2)))
      (if phase-complete
        (let ((next-phase (if (eq phase :offered) 0 (1+ phase))))
          (if (< next-phase (length phases))
              (let ((state (quest-phase-initial-state (elt phases next-phase))))
                (set-active-quest-state avatar label next-phase
                                        (if (listp state) (copy-list state) state))
                next-phase)
              (progn
                (remhash label (active-quests avatar))
                (sethash label (finished-quests avatar) (get-universal-time))
                (push label (dirty-quests avatar))
                :finished)))
        (set-active-quest-state avatar label phase new-state)))))

;;;

(defgeneric accept-quest (avatar quest npc)
  (:documentation "Called when `avatar' accepts `quest' from `npc'."))

(defmethod accept-quest :around (avatar quest npc)
  (let ((observers (list avatar npc))
        (quest (if (symbolp quest) (find-quest quest) quest)))
    (notify-observers observers :before-accept-quest avatar quest npc)
    (call-next-method)
    (notify-observers observers :after-accept-quest avatar quest npc)))

(defmethod accept-quest (avatar quest npc)
  (advance-quest avatar quest)
  (show-notice avatar "You have accepted the quest ~s from ~a."
               (quest-name quest)
               (describe-brief npc :article :definite))
  nil)  ; FIXME: (show-map avatar))

;;;

(defgeneric offer-quest (npc quest avatar)
  (:documentation "Called when `npc' offers `quest' to `avatar'."))

(defmethod offer-quest :around (npc quest avatar)
  (let ((observers (list avatar npc))
        (quest (if (symbolp quest) (find-quest quest) quest)))
    (notify-observers observers :before-offer-quest avatar quest npc)
    (call-next-method npc quest avatar)
    (notify-observers observers :after-offer-quest avatar quest npc)))

(defun accept-or-reject-quest (avatar quest npc accepted)
  "Called when `avatar' accepts or rejects an offer from `npc' to begin
`quest'."
  (if accepted
      (accept-quest avatar quest npc)
      (progn
        (show-notice avatar "You have rejected the quest ~s." (quest-name quest))
        (remhash (quest-label quest) (active-quests avatar))
        (show-map avatar))))

(defmethod offer-quest (npc quest avatar)
  (sethash (quest-label quest) (active-quests avatar) (list :offered nil))
  (make-offer avatar #'accept-or-reject-quest avatar quest npc)
  (show-notice avatar
               "~a has offered you the level ~d quest ~s. Type `accept` to accept it."
               (describe-brief npc :capitalize t :article :definite)
               (quest-level quest)
               (quest-name quest)))
