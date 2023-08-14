(in-package :jade)

;;; Combat traits represent values that impact an entity's combat performance.

(defgeneric compute-combat-traits (entity))

(defmethod compute-combat-traits (entity)
  nil)

(defmethod compute-combat-traits ((entity combatant))
  (plist-hash-table (? entity :traits)))

;;;

(defun level-scale (level)
  (let ((k 20))
    (float (/ (* level (+ k (1- level))) k))))

(defun scale-value-for-level (value level)
  (* value (level-scale level)))

(defun max-health (level traits)
  (round (scale-value-for-level (+ 10 (gethash :constitution traits 0)) level)))

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
   (scale-value-for-level
    (apply #'random-range (? attack :damage-range))
    (attack-level actor attack))))

;;; A combatant represents an entity that can enter combat.

(defclass combatant (entity)
  ((battle :initform nil :accessor battle)
   (current-target :initform nil :accessor current-target)
   (current-attack :initform nil :accessor current-attack)
   (attack-timer :initform nil :accessor attack-timer)
   (combat-traits :initform nil :accessor combat-traits)))

(defentity combatant (&class combatant)
  (:level 1
   :attacks nil
   :traits nil
   :attitude :neutral)  ; or :friendly, :hostile

  (:before-enter-world ()
    (print self)
    (let* ((traits (compute-combat-traits self))
           (max-health (max-health (? self :level) traits)))
      (setf (? self :max-health) max-health
            (? self :health) max-health
            (? self 'combat-traits) traits))))

(defmethod transform-initval ((name (eql :attacks)) value)
  `(mapcar #'symbol-value ',value))

(defmethod transform-initval ((name (eql :traits)) value)
  `(quote ,value))

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
      (:observers (? (location actor) :contents))
    (call-next-method)))

(defmethod attack ((actor combatant) (target combatant))
  (with-slots (attack-timer current-attack) actor
    (with-attributes (damage-range attack-verb) current-attack
      (let ((damage (attack-damage actor current-attack target)))
        ;; FIXME:
        (show actor "You ~a ~a with ~a for ~d damage!"
              (verb-plural attack-verb)
              (describe-brief target)
              (describe-brief current-attack)
              damage)
        (show target "~a ~a you with ~a for ~d damage!"
              (describe-brief actor :capitalize t)
              (verb-singular attack-verb)
              (describe-brief current-attack)
              damage))
      (setf attack-timer nil)
      (begin-attack actor target))))

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
    (let ((msg (format nil "~a has left the battle." (describe-brief actor :capitalize t))))
      (dolist (c (combatants battle))
        (show c msg)))
    (show actor "You are no longer in combat.")
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
