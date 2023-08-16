(in-package :jade)

;;; A combatant represents an entity that can enter combat.

(defclass combatant (entity)
  ((battle :initform nil :accessor battle)
   (current-target :initform nil :accessor current-target)
   (current-attack :initform nil :accessor current-attack)
   (attack-timer :initform nil :accessor attack-timer)
   (regen-interval :initform nil :accessor regen-interval)
   (cached-traits :initform (make-hash-table) :accessor cached-traits)))

(defentity combatant (&class combatant)
  (:level 1
   :base-health 10
   :attacks nil
   :traits nil
   :attitude :neutral))  ; or :friendly, :hostile

(defmethod transform-initval (class (name (eql :attacks)) value)
  "The `:attacks' attribute is a list of weapons/attacks which the combatant can
select during combat."
  (mapcar #'symbol-value value))

;;; Types of damage.

(defparameter *damage-types*
  (plist-hash-table
   (list
    ;; Physical
    :crushing '("crushing" "crushes" :crushing-resistance)
    :slashing '("slashing" "slashes" :slashing-resistance))))  ;; TODO: add more

(defun resistance-name (damage-type)
  (format nil "~a resistance" (car (gethash damage-type *damage-types*))))

;;; Combat traits.

;; In addition to those described below, damage types and their resistances are
;; also combat traits. Each point in a damage type (or "affinity") increases
;; effective attack level for related attacks by one percent. Similarly, Each
;; point in a resistance increases effective defense level for related attacks
;; by one percent.

(defparameter *combat-traits*
  (plist-hash-table
   (list
    ;; Each point of vitality increases maximum health by one percent.
    :vitality '("vitality")
    ;; Each point of precision increases the chance of scoring a critical hit by
    ;; 0.1 percent.
    :precision '("precision")
    ;; Each point of ferocity increases the effect of a critical hit by
    ;; 0.1 percent.
    :ferocity '("ferocity")))) ; TODO: add more

;;; Weights for equipment slots that contribute to the armor trait.

(defparameter *armor-slots*
  (plist-hash-table
   (list
    :head 0
    :torso 0
    :back 0
    :hands 0
    :waist 0
    :legs 0
    :feet)))

(defun armor-defense (avatar)
  "Returns the total defense provided to `avatar' by all items worn in armor
slots. This value is cached as the :armor trait."
  (apply #'+
         (maphash (lambda (slot weight)
                    (if-let ((item (? avatar :equipment slot)))
                      (* weight (? item :level) (or (? item :armor-modifier) 0))
                      0))
                  *armor-slots*)))

;;;

(defun attack-affinity (actor attack)
  (let ((damage-type (? attack :damage-type)))
    (gethash damage-type (cached-traits actor) 0)))

(defun attack-level (actor attack)
  (let* ((actor-level (? actor :level))
         (attack-level (or (? attack :level) actor-level))
         (affinity (attack-affinity actor attack)))
    (+ actor-level
       (* (1+ (* affinity 0.01)) attack-level))))

;;; Defense. Note that the defense trait describes natural defense, and the
;;; armor trait describes defense gained from wearing armor.

(defun attack-resistance (target attack)
  (let ((damage-type (? attack :damage-type)))
    (gethash damage-type (cached-traits target) 0)))

(defun defense-level (target attack)
  (with-slots (cached-traits) target
    (let ((target-level (? target :level))
          (armor (gethash :armor cached-traits 0))
          (defense (gethash :defense cached-traits 0))
          (resistance (attack-resistance target attack)))
      (+ target-level
         (* (1+ (* resistance 0.01)) (+ armor defense))))))

;;;

(defun smoothstep (x &optional (from 0.0) (to 1.0))
  (let ((x (max 0.0 (min 1.0 (/ (- x from) (- to from))))))
    (* x x (- 3.0 (* x 2.0)))))

(defun attack-effectiveness (att def)
  (* 2.0 (smoothstep (- att def) -20 20)))

;;;

(defun roll-damage (actor attack)
  (with-attributes (level base-damage damage-variance) attack
    (let* ((actor-level (? actor :level))
           (level (if level (* 0.5 (+ actor-level level)) actor-level))
           (k (* base-damage damage-variance)))
      (* (1+ (* 0.25 (1- level)))
         (random-range (- base-damage k) (+ base-damage k))))))

(defun resolve-attack (actor attack target)
  "Computes the damage done by an instance of `actor' using `attack' against
`target'. The secondary return value is true if the attack was a critical hit."
  (with-slots (cached-traits) actor
    (let* ((att (attack-level actor attack))
           (def (defense-level target attack))
           (eff (attack-effectiveness att def))
           (damage (* eff (roll-damage actor attack)))
           (crit-chance (+ 0.05 (* 0.001 (gethash :precision cached-traits 0)))))
      (if (< (random 1.0) crit-chance)
          (values (round-random (* damage
                                   (+ 1.5 (* 0.01 (gethash :ferocity cached-traits 0)))))
                  t)
          (values (round-random damage) nil)))))

;;;

(defun base-health (combatant)
  (or (? combatant :race :base-health) (? combatant :base-health) 1))

(defun max-health (combatant)
  (floor (* (1+ (* 0.25 (1- (? combatant :level))))
            (base-health combatant)
            (1+ (* 0.01 (gethash :vitality (cached-traits combatant) 0))))))

;;; Update cached traits for a combatant.

(defgeneric merge-traits (entity cache))

(defmethod merge-traits ((entity entity) cache)
  (loop for (trait value) on (? entity :traits) by #'cddr do
    (incf (gethash trait cache 0) value)))

(defun update-cached-traits (combatant)
  (with-slots (cached-traits) combatant
    (clrhash cached-traits)
    (merge-traits combatant cached-traits)))

;;; Regeneration.

(defparameter *regen-tick-seconds* 3)

(defgeneric regenerate (actor))

(defmethod regenerate :around ((actor combatant))
  (unless (battle actor)
    (process-simple-event regenerate nil
        (:observers (cons (location actor) (? (location actor) :contents)))
      (call-next-method))))

(defmethod regenerate ((actor combatant))
  (setf (? actor :health)
        (min (? actor :max-health)
             (+ (? actor :health) (base-health actor)))))

;;;

(defmethod enter-world ((actor combatant))
  (call-next-method)
  (update-cached-traits actor)
  (let ((max-health (max-health actor))
        (health (? actor :health)))
    (setf (? actor :max-health) max-health
          (? actor :health) (min (or health max-health) max-health)))
  (setf (regen-interval actor)
        (cl-async:with-interval (*regen-tick-seconds*)
          (regenerate actor))))

(defmethod exit-world ((actor combatant))
  (call-next-method)
  (with-slots (regen-interval) actor
      (cl-async:remove-interval regen-interval)
    (setf regen-interval nil)))

;;; An attack is any entity that defines the following attributes: level, speed,
;;; base-damage, damage-type, damage-variance, and attack-verb.

(defmethod transform-initval (class (name (eql :attack-verb)) value)
  (parse-verb value))

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
    (bind ((damage crit (resolve-attack actor current-attack target)))
      (declare (ignore crit)) ; FIXME: handle crits
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
