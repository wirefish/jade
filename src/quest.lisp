(in-package :jade)

#|

A quest moves through a sequence of phases as an avatar progresses through its
requirements, in the following order:

- :unavailable when the avatar has not finished the quest and does not meet the
  requirements to accept it;

- :available when the avatar can accept the quest;

- :offered when the quest has been offered to the avatar, but not yet accepted
  or rejected;

- "active" phases defined by the quest while the avatar is performing the quest;

- :finished when the avatar has completed the quest.

An avatar's active-quests slot contains a list of (quest-label phase-index
state) lists, with the most-recently-accepted quest first. The phase index and
state are updated as the avatar progresses through the quest. The entry is
removed when the avatar cancels or finishes a quest.

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
   (level :initarg :level :initform 1 :reader quest-level
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
                  :documentation "The state stored upon entering this phase.")))

(defun quest-get-phase (quest index)
  (elt (quest-phases quest) index))

(defun ensure-quest (arg)
  (ctypecase arg
    (quest arg)
    (symbol (symbol-value-as 'quest arg))))

(defmacro defquest (label attributes &body phases)
  (with-gensyms (quest)
    `(let ((,quest (make-instance
                    'quest
                    :label ',label
                    ,@(loop for (key value) on attributes by #'cddr
                            nconc (list key `(transform-initval 'quest ,key ',value)))
                    :phases
                    (list
                     ,@(loop for (label . attributes) in phases
                             collect `(make-instance
                                       'quest-phase
                                       :label ',label
                                       ,@(loop for (key value) on attributes by #'cddr
                                               nconc (list key
                                                           `(transform-initval 'quest
                                                                               ,key
                                                                               ',value)))))))))
       (set ',label ,quest)
       (export ',label)
       ,quest)))

;;;

(defun deactivate-quest (avatar quest-label)
  "Removes state for a quest from the avatar's active quests."
  (with-slots (active-quests) avatar
    (setf active-quests
          (delete-if (lambda (q) (eq (first q) quest-label))
                     active-quests)))
  (update-quests avatar quest-label))

(defun active-quest-state (avatar quest-label)
  "Returns (phase state) if the quest is active, or nil otherwise."
  (cdr (assoc quest-label (active-quests avatar))))

(defun can-accept-quest (avatar quest)
  (with-slots (label level required-quests can-accept) quest
    (and (null (active-quest-state avatar label))
         (not (gethash label (finished-quests avatar)))
         (>= (? avatar :level) level)
         (every (lambda (q) (gethash q (finished-quests avatar)))
                required-quests)
         (or (null can-accept) (funcall can-accept avatar)))))

(defun quest-offered (avatar quest-label)
  "Returns true if `avatar' has a pending offer to accept the specified quest."
  (when-let ((offer (pending-offer avatar)))
    (and (typep offer 'quest-offer)
         (eq quest-label (quest-label (slot-value offer 'quest))))))

(defun quest-phase (avatar quest-label)
  (cond
    ((not (typep avatar 'avatar))
     nil)
    ((gethash quest-label (finished-quests avatar))
     :finished)
    ((quest-offered avatar quest-label)
     :offered)
    (t
     (when-let ((quest (symbol-value-or-nil quest-label)))
       (if-let ((state (active-quest-state avatar quest-label)))
         (let ((phase (first state)))
           (if (numberp phase)
               (quest-phase-label (elt (quest-phases quest) phase))
               phase))
         (if (can-accept-quest avatar quest)
             :available
             :unavailable))))))

(defun set-active-quest-state (avatar quest-label phase &optional state)
  (let ((entry (assoc quest-label (active-quests avatar))))
    (setf (cdr entry) (list phase state))))

(defun begin-quest (avatar quest)
  (with-slots (label phases) quest
    (let ((state (quest-phase-initial-state (first phases))))
      (push (list label 0 (if (listp state) (copy-list state) state))
            (active-quests avatar)))
    (update-quests avatar label)))

(defparameter *default-remove-message* (parse-verb "is destroyed."))

(defun consume-quest-items (avatar label &key npc (message *default-remove-message*))
  "Removes all items associated with the quest named by `label' from the inventory
of `avatar'. If `npc' is not nil, makes it appear that items are given to `npc';
otherwise, `message' is a verb used to construct feedback to the player."
  (when-let ((items (remove-items-if avatar :inventory
                                     (lambda (i) (eq (? i :quest) label)))))
    (update-inventory avatar nil items)
    (let ((brief (format-list #'describe-brief items)))
      (if npc
          (show avatar "You give ~a to ~a."
                brief
                (describe-brief npc :article :definite))
          (let ((multiple (or (> (length items) 1)
                              (> (? (first items) :quantity) 1))))
            (show avatar
                  "~@(~a~) ~a"
                  brief
                  (if multiple (verb-plural message) (verb-singular message))))))))

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
       (if arg2
         (incf (cdr entry) arg2)
         (setf (cdr entry) 1))
       (values state (every (lambda (x) (>= (cdr x) 1)) state))))))

(defun advance-quest (actor avatar label &optional arg1 arg2)
  "Updates an avatar's state for an active quest and, if the current phase becomes
complete, advances to the next phase. Returns the index of the new phase, or
:finished if all phases have been completed."
  (let ((quest (symbol-value label)))
    (with-slots (phases xp-multiplier level) quest
      (bind (((phase &optional state) (active-quest-state avatar label))
             (new-state phase-complete (advance-quest-state state arg1 arg2)))
        (if phase-complete
            (let ((next-phase (1+ phase)))
              (if (< next-phase (length phases))
                  (let ((state (quest-phase-initial-state (elt phases next-phase))))
                    (set-active-quest-state avatar label next-phase
                                            (if (listp state) (copy-list state) state))
                    (show-notice avatar "You have progressed in the quest ~s!"
                                 (quest-name quest))
                    (show-map avatar)
                    (update-quests avatar label)
                    next-phase)
                  (progn
                    (consume-quest-items avatar label :npc actor)
                    (deactivate-quest avatar label)
                    (sethash label (finished-quests avatar) (get-universal-time))
                    (push label (dirty-quests avatar))
                    (show-notice avatar "You have completed the quest ~s!"
                                 (quest-name quest))
                    (show-map avatar)
                    (notify-observers (list* (location avatar)
                                             (? (location avatar) :contents))
                                      :after-finish-quest avatar quest)
                    (gain-xp avatar (round (* xp-multiplier (xp-granted-by-quest level))))
                    :finished)))
            (set-active-quest-state avatar label phase new-state))))))

;;;

(defgeneric accept-quest (avatar quest npc)
  (:documentation "Called when `avatar' accepts `quest' from `npc'."))

(defmethod accept-quest :around (avatar quest npc)
  (let ((observers (list avatar npc)))
    (notify-observers observers :before-accept-quest avatar quest npc)
    (call-next-method)
    (notify-observers observers :after-accept-quest avatar quest npc)))

(defmethod accept-quest (avatar quest npc)
  (begin-quest avatar quest)
  (show-notice avatar "You have accepted the quest ~s from ~a."
               (quest-name quest)
               (describe-brief npc :article :definite))
  nil)  ; FIXME: (show-map avatar))

;;;

(defclass quest-offer ()
  ((quest :initarg :quest)
   (npc :initarg :npc)))

(defmethod extend-offer (actor (offer quest-offer))
  (with-slots (quest npc) offer
    (show-notice actor
                 "~a has offered you the level ~d quest ~s. Type `accept` to accept it."
                 (describe-brief npc :capitalize t :article :definite)
                 (quest-level quest)
                 (quest-name quest))))

(defmethod accept-offer (actor (offer quest-offer))
  (with-slots (quest npc) offer
    (accept-quest actor quest npc)))

(defmethod reject-offer (actor (offer quest-offer))
  (with-slots (quest) offer
    (show-notice actor "You have rejected the quest ~s." (quest-name quest))))

;;;

(defgeneric offer-quest (npc quest avatar)
  (:documentation "Called when `npc' offers `quest' to `avatar'."))

(defmethod offer-quest :around (npc quest avatar)
  (let ((observers (list avatar npc))
        (quest (ensure-quest quest)))
    (notify-observers observers :before-offer-quest avatar quest npc)
    (call-next-method npc quest avatar)
    (notify-observers observers :after-offer-quest avatar quest npc)))

(defmethod offer-quest (npc quest avatar)
  (extend-offer avatar (make-instance 'quest-offer :quest quest :npc npc)))

;;;

(defmethod match-subject (tokens (subject quest))
  "Matches `tokens' against the name of `quest'."
  (match-subject tokens (quest-name subject)))

(defun match-active-quests (tokens active-quests)
  (find-matches tokens
                (mapcar (lambda (q) (symbol-value (first q))) active-quests)))

(defun summarize-active-quest (label phase state)
  ;; FIXME: include progress: what does it look like for list state?
  (declare (ignore state))
  (let ((quest (symbol-value-as 'quest label)))
    (with-slots (name level phases) quest
      (format nil "~a (level ~d): ~a"
              name
              level
              (quest-phase-summary (elt phases phase))))))

(defun drop-quest (actor quest)
  (with-slots (label name) quest
    (deactivate-quest actor label)
    (consume-quest-items actor label)
    (show-notice actor "You are no longer on the quest ~s." name)
    (show-map actor)))

(defcommand quest (actor ("quest" "qu") &word subcommand quest-name)
  "Display information about your active quests. This command has several
subcommands:

- `quest` or `quest list` displays a summary of your active quests and your
  progress toward their completion.

- `quest info *quest-name*` displays details for the named quest.

- `quest drop *quest-name*` drops the named quest. You will lose all progress
  toward completion of the quest and any associated items."
  (with-slots (active-quests) actor
    (cond
      ((or (null subcommand) (string-equal subcommand "list"))
       (if (null active-quests)
           (show actor "You are not on any quests.")
           (show actor "You are currently on the following quests:~%~%~{- ~a~%~%~}"
                 (loop for (label phase state) in active-quests
                       collect (summarize-active-quest label phase state)))))
      ((string-equal subcommand "info")
       (if quest-name
           (if-let ((quests (match-active-quests quest-name active-quests)))
             (dolist (quest quests)
               (show actor "~a (level ~d): ~a"
                     (quest-name quest) (quest-level quest) (quest-summary quest)))
             (show actor "You are not on any such quest."))
           (show actor "For which quest do you want to see more information?")))
      ((string-equal subcommand "drop")
       (if quest-name
           (let ((quests (match-active-quests quest-name active-quests)))
             (case (length quests)
               (0 (show actor "You are not on any such quest."))
               (1 (let ((quest (first quests)))
                    (drop-quest actor quest)
                    (show-map actor)))
               (t (show actor "Which quest do you want to drop: ~a?"
                        (format-list #'quest-name quests "or")))))
           (show actor "Which quest do you want to drop?")))
      ((string-equal subcommand "reset")
       (loop for quest in (mapcar (lambda (q) (symbol-value (first q))) active-quests) do
         (drop-quest actor quest))
       (clrhash (finished-quests actor))
       (show-map actor)
       (show-notice actor "Finished quests have been reset."))
      (t
       (show actor "Unknown subcommand. Type `help quest` for help.")))))
