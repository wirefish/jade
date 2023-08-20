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
   (level
    :initarg :level :initform 1 :reader skill-level
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
   (required-tool
    :initarg :required-tool :initform nil :reader skill-required-tool
    :documentation "A label defining the type of tool that must be equipped in
      order to exercise this skill.")
   (required-nearby
    :initarg :required-nearby :initform nil :reader skill-required-nearby
    :documentation "A label defining the type of object that must be present at
      the current location in order to exercise this skill.")
   (price
    :initarg :price :initform nil :reader skill-price
    :documentation "A list of the form (quantity currency) describing the
      currency required to learn the skill.")
   (karma
    :initarg :karma :initform 0 :reader skill-karma
    :documentation "The amount of karma required to learn the skill.")
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

(defmethod print-object ((skill skill) stream)
  (print-unreadable-object (skill stream :type t :identity t)
    (write (skill-label skill) :stream stream)))

(defmethod transform-initval ((class (eql 'skill)) (name (eql :price)) value)
  (bind (((quantity currency) value))
    (clone-entity currency :quantity quantity)))

;;;

(defmacro defskill (label attributes)
  (with-gensyms (skill)
    `(let ((,skill (make-instance
                    'skill
                    :label ',label
                    ,@(loop for (key value) on attributes by #'cddr
                            nconc (list key `(transform-initval 'skill ,key ',value))))))
       (set ',label ,skill)
       (export ',label)
       ,skill)))

;;;

(defmethod match-subject (tokens (subject skill))
  (match-subject tokens (skill-name subject)))

;;;

(defun skill-rank (avatar skill &optional default)
  (gethash skill (skills avatar) default))

(defun skill-rank-increase (current-rank difficulty)
  "Returns the amount by which to increase an avatar's rank in a skill,
based on the avatar's current rank and the difficulty of the task being
performed. Difficulty depends on context; it could be computed from the level of
an adversary for a combat skill, or be the required rank of a resource for a
gathering skill, etc."
  (if (<= current-rank difficulty)
      1
      (let ((k (floor (/ (- current-rank difficulty) 4))))
        (/ 1 (expt 2 k)))))

(defun increase-skill-rank (avatar label difficulty)
  (when-let* ((rank (skill-rank avatar label))
              (skill (symbol-value-as 'skill label nil)))
    (let ((new-rank (+ rank (skill-rank-increase rank difficulty))))
      (sethash label (skills avatar) new-rank)
      (when (> (floor new-rank) (floor rank))
        (show-notice avatar "Your rank in ~a is now ~d!"
                     (skill-name skill)
                     (floor new-rank))
        (update-skills avatar label)))))

;;; A trainer is an entity with a `:teaches' attribute, which is a list of
;;; labels indicating the skills that can be learned from the trainer. A trainer
;;; also has a `:level' attribute which potentially limits the skills it can
;;; actually teach; if `:level' is t, the trainer can teach all skills.

(defclass trainer (entity) ())

(defentity trainer (&class trainer)
  (:brief "a skill trainer"
   :teaches nil
   :level t))

(defmethod transform-initval ((class (eql 'trainer)) (name (eql :teaches)) value)
  (stable-sort (remove-if #'null (mapcar #'symbol-value (ensure-list value)))
               #'<
               :key #'skill-level))

;;;

(defgeneric learn-skill (avatar skill trainer))

(defmethod learn-skill :around (avatar skill trainer)
  (process-simple-event learn-skill (avatar skill trainer)
      (:observers (? (location avatar) :contents))
    (call-next-method)))

(defmethod learn-skill (avatar skill trainer)
  ;; FIXME: deduct karma and price.
  (setf (gethash (skill-label skill) (skills avatar)) 1)
  (update-skills avatar (skill-label skill))
  (if trainer
      (show-notice avatar "~a teaches you ~a."
                   (describe-brief trainer :article :definite :capitalize t)
                   (skill-name skill))
      (show-notice avatar "You learn ~a." (skill-name skill))))

;;;

(defun teachable-skills (trainer)
  (let ((trainer-level (? trainer :level)))
    (if (eq trainer-level t)
        (? trainer :teaches)
        (remove-if-not (lambda (s) (>= trainer-level (skill-level s)))
                       (? trainer :teaches)))))

(defun match-trainer-skills (tokens trainers)
  "Returns a list of (trainer . skill) for the skills taught by `trainers' that
best match `tokens'."
  (apply #'find-matches tokens
         (loop for trainer in trainers
               collect (mapcar #'(lambda (skill) (cons trainer skill))
                               (teachable-skills trainer)))))

(defcommand learn (actor "learn" skill-name)
  "When no *skill-name* is given, list the skills taught by nearby trainers.
Otherwise, try to learn the specified skill."
  (if-let ((trainers (remove-if-not (lambda (x) (typep x 'trainer))
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
                  (show actor "You have already learned ~a."
                        (skill-name skill)))
                 ((< (? actor :level) (skill-level skill))
                  (show actor "You must reach at least level ~d before learning ~a."
                        (skill-level skill)
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
          (if-let ((skills (teachable-skills trainer)))
            (show-trainer-skills
             actor
             (format nil "~a teaches the following skills:"
                     (describe-brief trainer :article :definite :capitalize t))
             trainer skills)
            (show actor "~a does not teach any skills."
                  (describe-brief trainer :article :definite :capitalize t)))))
    (show actor "There are no skill trainers here.")))

;;;

(defgeneric unlearn-skill (avatar skill trainer))

(defmethod unlearn-skill :around (avatar skill trainer)
  (process-simple-event unlearn-skill (avatar skill trainer)
      (:observers (? (location avatar) :contents))
    (call-next-method)))

(defmethod unlearn-skill (avatar skill trainer)
  ;; FIXME: refund karma cost.
  (remhash (skill-label skill) (skills avatar))
  (update-skills avatar (skill-label skill))
  (show-notice avatar "You unlearn ~a." (skill-name skill)))

;;;

(defun match-known-skill (avatar tokens)
  (match avatar tokens :exactly-one (mapcar #'symbol-value (hash-table-keys (skills avatar)))
   :no-tokens "Which skill do you want to unlearn?"
   :no-subjects "You haven't learned any skills."
   :no-match "You haven't learn any skill matching ~s."
   :multi-match "Do you want to unlearn ~a?"))

(defcommand unlearn (actor "unlearn" skill-name)
  "Unlearn a skill you previously learned. This can only be done at a trainer who
teaches the skill. Unlearning a skill refunds any karma spent on the skill, but
other costs are not refunded."
  (when-let ((skill (match-known-skill actor skill-name)))
    (let ((trainers (remove-if-not (lambda (x) (typep x 'trainer))
                                   (? (location actor) :contents))))
      (if-let ((trainer (some #'(lambda (x) (find skill (teachable-skills x)))
                              trainers)))
        (unlearn-skill actor skill trainer)
        (show actor
              "There is no trainer nearby who can help you unlearn ~a."
              (skill-name skill))))))

;;;

(defcommand skills (actor ("skills" "skill" "sk") skill-name)
  "View information about skills you've learned. If *skill-name* is specified,
view details for matching skills. Otherwise, view the names of all the skills
you have learned.

For more information see `help:learn` and `help:unlearn`."
  (if skill-name
      ;; Show details for matching skills.
      (if-let ((matches (find-matches skill-name
                                      (mapcar #'symbol-value
                                              (hash-table-keys (skills actor))))))
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
            (let ((names (mapcar (compose #'skill-name #'symbol-value)
                                 (hash-table-keys skills))))
              (show-links actor "You have learned the following skills:" "skills"
                          (sort names #'string<)))
            (show actor "You haven't learned any skills yet.")))))
