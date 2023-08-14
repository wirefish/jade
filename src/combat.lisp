(in-package :jade)

;;;

(defun level-scale (level &key (rate 20))
  "Returns a multiplier that scales a value based on `level'. A `rate' of 1 causes
a geometric progression, i.e. the return value is (* level level). A higher
value for `rate' will cause the scale value to grow more slowly, approaching a
linear progression as `rate' becomes very large."
  (float (/ (* level (+ rate (1- level))) rate)))

(defun attack-level (actor attack)
  (let ((actor-level (or (? actor :level) 1))
        (attack-level (? attack :level)))
    (if (null attack-level)
        actor-level
        (/ (+ actor-level attack-level) 2))))

(defun attack-damage (actor attack target)
  ;; FIXME: take target's level and defense into account.
  (declare (ignore target))
  (round-random
   (* (level-scale (attack-level actor attack))
      (apply #'random-range (? attack :damage-range)))))

;;; A combatant represents an entity that can enter combat.

(defclass combatant (entity)
  ((battle :initform nil :accessor battle)
   (current-target :initform nil :accessor current-target)
   (current-attack :initform nil :accessor current-attack)
   (attack-timer :initform nil :accessor attack-timer)
   (regen-interval :initform nil)
   (combat-traits :initform nil :accessor combat-traits)))

(defgeneric compute-combat-traits (entity)
  (:method (entity)))

(defmethod compute-combat-traits ((entity combatant))
  (plist-hash-table (? entity :traits)))

(defgeneric base-health (entity)
  (:method ((entity combatant))
    (+ (? entity :base-health)
       (gethash :vitality (combat-traits entity) 0))))

(defun max-health (combatant)
  (round (* (base-health combatant) (level-scale (? combatant :level)))))

;;; Regeneration.

(defparameter *regen-tick-seconds* 3)

(defgeneric regenerate (actor))

(defmethod regenerate :around ((actor combatant))
  (unless (battle actor)
    (process-simple-event regenerate nil
        (:observers (cons (location actor) (? (location actor) :contents)))
      (call-next-method))))

(defun health-regen-per-tick (combatant)
  (round (level-scale (? combatant :level) :rate 100)))

(defmethod regenerate ((actor combatant))
  (setf (? actor :health)
        (min (? actor :max-health)
             (+ (? actor :health) (health-regen-per-tick actor)))))

;;;

(defentity combatant (&class combatant)
  (:level 1
   :base-health 10
   :attacks nil
   :traits nil
   :attitude :neutral))  ; or :friendly, :hostile

(defmethod transform-initval ((name (eql :attacks)) value)
  `(mapcar #'symbol-value ',value))

(defmethod transform-initval ((name (eql :traits)) value)
  `(quote ,value))

;;;

(defmethod enter-world ((actor combatant))
  (call-next-method)
  (with-slots (combat-traits regen-interval) actor
    (setf combat-traits (compute-combat-traits actor))
    (let ((max-health (max-health actor)))
      (setf (? actor :max-health) max-health
            (? actor :health) max-health))
    (setf regen-interval
          (cl-async:with-interval (*regen-tick-seconds*)
            (regenerate actor)))))

(defmethod exit-world ((actor combatant))
  (call-next-method)
  (with-slots (regen-interval) actor
      (cl-async:remove-interval regen-interval)
    (setf regen-interval nil)))

;;; An attack is any entity that defines the following attributes: level, speed,
;;; damage-type, damage-range, and attack-verb.

(defmethod transform-initval ((name (eql :attack-verb)) value)
  `(parse-verb ,value))

;;; A battle represents a fight pitting all :friendly combatants against all
;;; :neutral or :hostile combatants.

(defclass battle ()
  ((combatants :initarg :combatants :initform nil :accessor combatants)))

(defun enemies (actor battle)
  "Returns a list of combatants which are opposed to `actor' in `battle'."
  (with-attributes (attitude) actor
    (loop for combatant in (combatants battle)
          if (if (eq attitude :friendly)
                 (position (? combatant :attitude) '(:neutral :hostile))
                 (eq (? combatant :attitude) :friendly))
            collect combatant)))

;;; The generic functions that implement combat and related events.

(defgeneric enter-combat (actor target))

(defgeneric select-attack (actor target))

(defgeneric attack (actor target))

(defgeneric kill (actor target))

(defgeneric exit-combat (actor &key force))

;;;

(defmethod select-attack ((actor combatant) target)
  (random-elt (? actor :attacks)))

;;;

(defun begin-attack (actor target)
  (with-slots (current-target current-attack attack-timer) actor
    (when attack-timer
      (cl-async:remove-event attack-timer))
    (setf current-target target
          current-attack (select-attack actor target)
          attack-timer (if current-attack
                           (with-delay ((or (? current-attack :speed) 5))
                             (attack actor target))
                           (with-delay (5)
                             (begin-attack actor target))))))

(defmethod attack (actor target)
  (show actor "You can't attack ~a." (describe-brief target)))

(defmethod attack :around ((actor combatant) (target combatant))
  (process-simple-event attack (actor target)
      (:observers (cons (location actor) (? (location actor) :contents)))
    (call-next-method)))

(defgeneric describe-attack (observer actor target attack damage)
  (:method (observer actor target attack damage)))

(defmethod attack ((actor combatant) (target combatant))
  (with-slots (attack-timer current-attack) actor
    (let ((damage (attack-damage actor current-attack target)))
      (when (> (? target :health) 0)
        (when (<= (decf (? target :health) damage) 0)
          ;; The target dies but not until after this event has been fully
          ;; processed.
          (with-delay (0)
            (kill actor target))))
      (show-observers (? (location actor) :contents)
                      (lambda (e) (describe-attack e actor target current-attack damage))))
    (setf attack-timer nil)
    (begin-attack actor target)))

;;;

(defmethod kill :around ((actor combatant) (target combatant))
  (process-simple-event kill (actor target)
      (:observers (observer-list* target (location actor) (? (location actor) :contents)))
    (call-next-method)))

(defgeneric describe-death (observer actor target)
  (:method (observer actor target)))

(defmethod kill ((actor combatant) (target combatant))
  (show-observers (? (location actor) :contents)
                  (lambda (e) (describe-death e actor target)))
  ;; FIXME: create a 'death' portal to provide a better message.
  (exit-location target (location target) nil :force t)
  (exit-world target))

;;;

(defun enter-battle (battle &rest combatants)
  (dolist (combatant combatants)
    (setf (battle combatant) battle))
  (dolist (combatant combatants)
    (show combatant "You enter combat against ~a!"
          (format-list #'describe-brief (enemies combatant battle)))))

(defmethod enter-combat (actor target)
  (show actor "You can't attack ~a."))

(defmethod enter-combat :around ((actor combatant) (target combatant))
  (process-simple-event enter-combat (actor target)
      (:observers (? (location actor) :contents))
    (call-next-method)))

(defmethod enter-combat ((actor combatant) (target combatant))
  (with-slots ((actor-battle battle)) actor
    (with-slots ((target-battle battle)) target
      (cond
        ((and (null actor-battle) (null target-battle))
         ;; Create a new battle between just actor and target.
         (let ((battle (make-instance 'battle :combatants (list actor target))))
           (enter-battle battle actor target)
           (begin-attack actor target)
           (unless (current-target target)
             (begin-attack target actor))))
        ((and actor-battle target-battle)
         ;; TODO: merge the two battles.
         )
        (t
         ;; TODO: Add actor or target to the other's existing battle.
         )))))

;;;

(defun exit-battle (actor)
  (format-log :info "~a is leaving battle" actor)
  (with-slots (battle current-target attack-timer) actor
    (deletef (combatants battle) actor)
    (when attack-timer
      (cl-async:remove-event attack-timer))
    (when (> (? actor :health) 0)
      (show-observers
       (combatants battle)
       (format nil "~a has left the battle." (describe-brief actor :capitalize t)))
      (show actor "You are no longer in combat."))
    (setf current-target nil attack-timer nil battle nil)
    battle))

(defmethod exit-combat (actor &key force)
  (declare (ignore force))
  t)

(defmethod exit-combat :around ((actor combatant) &key force)
  (with-slots (battle) actor
    (if battle
        (process-simple-event exit-combat (actor)
            (:observers (? (location actor) :contents)
             :force force)
          (call-next-method))
        t)))

(defmethod exit-combat ((actor combatant) &key force)
  (declare (ignore force))
  (let ((battle (battle actor)))
    (exit-battle actor)
    ;; FIXME: dissolve the battle if all remaining combatants are on the same side.
    (when (= (length (combatants battle)) 1)
      (exit-combat (first (combatants battle)) :force t))))
