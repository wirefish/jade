(in-package :jade)

;;;

(defclass skill ()
  ((label
    :initarg :label :initform nil :reader skill-label
    :documentation "The symbol to bind to the skill object.")
   (name
    :initarg :name :initform nil :reader skill-name
    :documentation "The player-visible name of the skill.")
   (summary
    :initarg :summary :initform nil :reader skill-summary
    :documentation "A one-sentence description of what the skill enables the
      player to do.")
   (required-level
    :initarg :required-level :initform 1 :reader skill-required-level
    :documentation "The minimum level required to learn this skill.")
   (required-race
    :initarg :required-race :initform nil :reader skill-required-race
    :documentation "The race that can learn this skill, or nil if there is no
      racial requirement.")
   (required-skills
    :initarg :required-skills :initform nil :reader skill-required-skills
    :documentation "A plist of labels and ranks of skills that must be learned
      before learning this skill.")
   (exclusive-skills
    :initarg :exclusive-skills :initform nil :reader skill-exclusive-skills
    :documentation "Skills that must not be known when learning this skill.")
   (cost
    :initarg :cost :initform nil :reader skill-cost
    :documentation "A plist of symbols and amounts of currency required to learn
      this skill, where :karma is a valid currency.")
   (max-rank
    :initarg :max-rank :initform 100 :reader skill-max-rank
    :documentation "The maximum rank that can be attained in this skill.")
   (traits
    :initarg :traits :initform nil :reader skill-traits
    :documentation "A plist of traits conferred upon an avatar who knows this
      skill.")
   (recipes
    :initarg :recipes :initform nil :reader skill-recipes
    :documentation "A list of recipes made available upon learning this
      skill.")))

;;;

(defmacro defskill (label attributes)
  (with-gensyms (skill)
    `(let ((,skill (make-instance
                    'skill
                    :label ',label
                    ,@(loop for (key value) on attributes by #'cddr
                            nconc (list key `(transform-initval 'quest ,key ',value))))))
       (set ',label ,skill)
       (export ',label)
       ,skill)))

;;;

(defmethod match-subject (tokens (subject skill))
  (match-subject tokens (skill-name subject)))

;;;

(defun skill-rank-increase (current-rank difficulty)
  "Returns the amount by which to increase an avatar's rank in a skill,
based on the avatar's current rank and the difficulty of the task being
performed. Difficulty depends on context; it could be computed from the level of
an adversary for a combat skill, or be the required rank of a resource for a
gathering skill, etc."
  (if (<= current-rank difficulty)
      1.0
      (max 0.0 (- 1.0 (/ (- current-rank difficulty) 20)))))

;;;

(defgeneric learn-skill (avatar skill trainer))

(defmethod learn-skill :around (avatar skill trainer)
  (process-simple-event learn-skill (avatar skill trainer)
      (:observers (? (location avatar) :contents))
    (call-next-method)))

(defmethod learn-skill (avatar skill trainer)
  ;; FIXME: deduct the cost.
  (update-skills avatar (skill-label skill))
  (setf (gethash (skill-label skill) (skills avatar)) 1)
  (if trainer
      (show-notice avatar "~a teaches you the skill ~s."
                   (describe-brief trainer :article :definite :capitalize t)
                   (skill-name skill))
      (show-notice avatar "You learn the skill ~s." (skill-name skill))))

;;;

(defun match-trainer-skills (tokens trainers)
  "Returns a list of (trainer . skill) for the skills taught by `trainers' that
best match `tokens'."
  (apply #'find-matches tokens
         (loop for trainer in trainers
               collect (mapcar #'(lambda (x) (cons trainer (symbol-value x)))
                               (? trainer :teaches)))))

(defcommand learn (actor "learn" skill-name)
  "When no *skill-name* is given, list the skills taught by nearby trainers.
Otherwise, try to learn the specified skill."
  (if-let ((trainers (remove-if-not (lambda (x) (? x :teaches))
                                    (? (location actor) :contents))))
    (if skill-name
        ;; Try to learn the skill
        (let ((matches (match-trainer-skills skill-name trainers)))
          (case (length matches)
            (0
             (show actor "Nobody here teaches a skill that matches ~s."
                   (join-tokens skill-name)))
            (1
             (destructuring-bind (trainer . skill) (first matches)
               (cond
                 ((gethash (skill-label skill) (skills actor))
                  (show actor "You had already learned ~s."
                        (skill-name skill)))
                 ((< (? actor :level) (skill-required-level skill))
                  (show actor "You must reach at least level ~d before learning ~s."
                        (skill-required-level skill)
                        (skill-name skill)))
                 ((and (skill-required-race skill)
                       (not (eq (skill-required-race skill) (entity-label (? actor :race)))))
                  (show actor "You must be ~a in order to learn ~s."
                        (describe-brief (symbol-value (skill-required-race skill)))
                        (skill-name skill)))
                 (t
                  (learn-skill actor skill trainer)))))
            (t
             (show actor "Do you want to learn ~a?"
                   (format-list #'skill-name (mapcar #'cdr matches) "or")))))
        ;; List all available skills.
        (dolist (trainer trainers)
          (show actor "~a teaches the following skills:"
                (describe-brief trainer :article :definite :capitalize t))
          (dolist (label (? trainer :teaches))
            (when-let ((skill (symbol-value label)))
              (show actor "- `learn:~a`: ~a Level ~d. Cost: ~{~(~a~) ~d~^, ~}."
                    (skill-name skill)
                    (skill-summary skill)
                    (skill-required-level skill)
                    (skill-cost skill))))))
    (show actor "There are no skill trainers here.")))

;;;

(defgeneric unlearn-skill (avatar skill trainer))

(defmethod unlearn-skill :around (avatar skill trainer)
  (process-simple-event unlearn-skill (avatar skill trainer)
      (:observers (? (location avatar) :contents))
    (call-next-method)))

(defmethod unlearn-skill (avatar skill trainer)
  ;; FIXME: refund karma cost.
  (update-skills avatar (skill-label skill))
  (remhash (skill-label skill) (skills avatar))
  (show-notice avatar "You unlearn the skill ~s." (skill-name skill)))

;;;

(defcommand unlearn (actor "unlearn" skill-name)
  "Unlearn a skill you previously learned. This can only be done at a trainer who
teaches the skill. Unlearning a skill refunds any karma spent on the skill, but
other costs are not refunded."
  (if skill-name
      (let ((matches (find-matches skill-name
                                   (maphash-keys #'symbol-value (skills actor)))))
        (case (length matches)
          (0
           (show actor "You don't know any skills that match ~s."
                 (join-tokens skill-name)))
          (1
           (let ((skill (first matches)))
             (if-let ((trainer (some #'(lambda (x)
                                         (when (find (skill-label skill) (? x :teaches)) x))
                                     (? (location actor) :contents))))
               (unlearn-skill actor skill trainer)
               (show actor
                     "There is no trainer nearby who can help you unlearn that skill."))))
          (t
           (show actor "Do you want to unlearn ~a?"
                 (format-list #'skill-name matches "or")))))
      (show actor "Which skill do you want to unlearn?")))

;;;

(defcommand skills (actor ("skills" "skill" "sk") skill-name)
  "View information about skills you've learned. If *skill-name* is specified,
view details for matching skills. Otherwise, view a list of all the skills you
have learned.

For more information see `help:learn` and `help:unlearn`."
  (if skill-name
      ;; Show details for matching skills.
      (if-let ((matches (find-matches skill-name
                                      (maphash-keys #'symbol-value (skills actor)))))
        (dolist (skill matches)
          (let ((rank (gethash (skill-label skill) (skills actor))))
            (show actor "- ~a~a: ~a"
                  (skill-name skill)
                  (if (> (skill-max-rank skill) 0)
                      (format nil " (rank ~d/~d)" (floor rank) (skill-max-rank skill))
                      "")
                  (skill-summary skill))))
        (show actor "You dont't know any skills that match ~s."
              (join-tokens skill-name)))
      ;; List all skills.
      (with-slots (skills) actor
        (if (> (hash-table-count skills) 0)
            (show-links actor "You have learned the following skills:" "skills"
                        (sort (maphash-keys #'(lambda (x) (skill-name (symbol-value x)))
                                            skills)
                              #'string<))
            (show actor "You haven't learned any skills yet.")))))
