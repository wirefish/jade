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
   :attitude :neutral ; or :friendly, :hostile
   :leaves-corpse t))

(defmethod transform-initval (class (name (eql :attacks)) value)
  "The `:attacks' attribute is a list of weapons/attacks which the combatant can
select during combat."
  (mapcar #'symbol-value value))

(defmethod transform-initval (class (name (eql :loot)) value)
  "The `:loot' attribute describes a generator used to determine loot dropped
when the combatant dies."
  (make-generator value))

;;; A corpse is an entity that appears when a combatant dies. It can act as a
;;; resource node for e.g. a skinning skill, as well as drop loot when used.

(defclass corpse (resource-node)
  ((entity :initform nil :accessor corpse-entity)
   (owners :initform nil :accessor corpse-owners)
   (decay-timer :initform nil :accessor corpse-decay-timer)))

(defentity corpse (&class corpse)
  (:brief "a corpse of ~a"
   :description "The corpse is in bad shape ... it might decay at any time."
   :icon tombstone
   :decay-time 60
   :entry-message nil
   :exit-message "decays."))

(defmethod describe-brief ((corpse corpse) &key quantity (article :indefinite) capitalize)
  (declare (ignore quantity article capitalize))
  (format nil (call-next-method) (describe-brief (corpse-entity corpse))))

(defmethod match-subject (tokens (corpse corpse))
  (match-subject tokens (describe-brief corpse :article nil)))

(defmethod enter-world ((corpse corpse))
  (call-next-method)
  (setf (corpse-decay-timer corpse)
        (with-delay ((? corpse :decay-time))
          (despawn-entity corpse))))

(defmethod exit-world ((corpse corpse))
  (call-next-method)
  (as:remove-event (corpse-decay-timer corpse)))

;;; Combat traits. Each trait's value is computed based on equipment, race, etc.
;;; and is cached in a combatant's `cached-traits' slot. Each trait has a
;;; specific effect as defined by the functions below.

(defparameter *combat-traits*
  (plist-hash-table
   '(:vitality "vitality"
     :precision "precision"
     :ferocity "ferocity"
     :alacrity "alacrity")))

(defun combat-trait-name (trait)
  (gethash trait *combat-traits*))

(defun softcap (n k)
  (if (<= n k)
      n
      (let ((x (float (- n k))))
        (+ k (/ x (1+ (/ x k)))))))

(defun health-bonus (combatant)
  "Each point of :vitality increases maximum health by 1%."
  (1+ (softcap (* 0.01 (gethash :vitality (cached-traits combatant) 0))
               1.0)))

(defun critical-chance-bonus (combatant)
  "Each point of :precision increases critical hit chance by 0.2%."
  (softcap (* 0.002 (gethash :precision (cached-traits combatant) 0))
           0.5))

(defun critical-damage-bonus (combatant)
  "Each point of :ferocity increases critical hit damage by 1%."
  (softcap (* 0.01 (gethash :ferocity (cached-traits combatant) 0))
           1.0))

(defun attack-speed-bonus (combatant)
  "Each point of :alacrity reduces auto attack delay by 0.5%."
  (softcap (* 0.005 (gethash :alacrity (cached-traits combatant) 0))
           0.25))

;;; Damage types. The game world code should use `define-damage-types' to define
;;; them. Damage types and their resistances are also considered combat traits
;;; and are cached in the same way. Each point in a damage type (here called an
;;; "affinity") increases effective attack level for related attacks by one
;;; percent. Similarly, each point in a resistance increases effective defense
;;; level for related attacks by one percent.

(defstruct damage-type
  name verb resistance)

(defparameter *damage-types* (make-hash-table))

(defun add-damage-type (key name verb resistance)
  (sethash key *damage-types*
           (make-damage-type :name name :verb (parse-verb verb) :resistance resistance))
  (sethash key *combat-traits* name)
  (sethash resistance *combat-traits* (format nil "~a resistance" name)))

(defmacro define-damage-types (&body damage-types)
  "Defines damage types. Each element of `damage-types' is a list containing a
symbol to represent the type, a string to name the type, and a verb to describe
the effect of the type. For example, (fire \"fire\" \"burns\")."
  (let* ((keys (mapcar #'first damage-types))
         (resistances (mapcar (lambda (key)
                                (format-symbol (symbol-package key) "~a-RESISTANCE" key))
                              keys)))
    `(progn
       ,@(loop for (key name verb) in damage-types for resistance in resistances
               collect `(add-damage-type ',key ,name ,verb ',resistance))
       (export '(,@keys ,@resistances)))))

(defun attack-verb (damage-type)
  (when-let ((dt (gethash damage-type *damage-types*)))
    (damage-type-verb dt)))

(defun affinity-bonus (combatant damage-type)
  (if damage-type
      (softcap (* 0.01 (gethash damage-type (cached-traits combatant) 0))
               1.0)
      0))

(defun resistance-bonus (combatant damage-type)
  (if damage-type
      (let ((resistance (damage-type-resistance (gethash damage-type *damage-types*))))
        (softcap (* 0.01 (gethash resistance (cached-traits combatant) 0))
                 1.0))
      0))

(defun describe-traits (traits)
  (when traits
    (loop for trait being the hash-keys in traits using (hash-value value)
          collect
          (format nil "~a ~d" (gethash trait *combat-traits*) value))))

;;; Armor.

;; Weights for equipment slots that contribute to the armor trait. The values
;; must add up to one.
(defparameter *armor-slots*
  (plist-hash-table
   (list
    :head 3/16
    :torso 5/16
    :hands 2/16
    :legs 4/16
    :feet 2/16
    )))

(defun armor-value (item)
  "Returns the defense contributed by a piece of armor, or nil if the item is
not equippable as armor."
  (when-attributes (level equippable-slot armor-multiplier) item
    (when-let ((weight (gethash equippable-slot *armor-slots*)))
      (* 10 (float weight) level armor-multiplier))))

(defun armor-defense (avatar)
  "Returns the total defense provided to `avatar' by all items worn in armor
slots. This value is cached as the :armor trait."
  (apply #'+
         (maphash* (lambda (slot weight)
                     (if-let ((item (? avatar :equipment slot)))
                       (* (float weight) (? item :level) (or (? item :armor-multiplier) 0))
                       0))
                   *armor-slots*)))

(defun armor-speed-multiplier (avatar)
  "Returns the total speed multiplier for `avatar' due to all items worn in
armor slots. This value is cached as the :armor-speed trait."
  (apply #'+
         (maphash* (lambda (slot weight)
                     (* weight (or (? avatar :equipment slot :speed-multiplier) 1.0)))
                   *armor-slots*)))

;;; Attack rating. The value is based on the actor's level, the attack level
;;; (which defaults to the actor's level if null, e.g. for natural weapons), the
;;; actor's proficiency or mastery with the attack, and the actor's affinity for
;;; the attack's damage type.

(defun attack-rating (actor attack)
  (with-slots (skills) actor
    (let* ((actor-level (? actor :level))
           (attack-level
             (* (or (? attack :level) actor-level)
                (if-let ((proficiency (? attack :proficiency)))
                  (cond
                    ((gethash (? attack :mastery) skills) 1.25)
                    ((gethash proficiency skills) 1.0)
                    (t 0.5))
                  1.0))))
      (* 10
         (1+ (affinity-bonus actor (? attack :damage-type)))
         (+ actor-level attack-level)))))

;;; Defense rating. Note that the defense trait describes natural defense, and
;;; the armor trait describes defense gained from wearing armor.

(defun defense-rating (target attack)
  (with-slots (cached-traits) target
    (let ((target-level (? target :level))
          (armor (gethash :armor cached-traits 0))
          (defense (gethash :defense cached-traits 0)))
      (* 10
         (1+ (resistance-bonus target (? attack :damage-type)))
         (+ target-level armor defense)))))

;;; Attack timing.

(defun auto-attack-delay (actor attack)
  "Returns the time between auto attacks by `actor' with `attack', in seconds."
  (* (? attack :speed)
     (gethash :armor-speed (cached-traits actor) 1.0)
     (- 1.0 (attack-speed-bonus actor))))

;;; Computing damage for an attack.

(defun smoothstep (x &optional (from 0.0) (to 1.0))
  "A function whose value smoothly transitions from 0 when `x' <= `from' to 1 when
`x' >= `to'."
  (let ((x (max 0.0 (min 1.0 (/ (- x from) (- to from))))))
    (* x x (- 3.0 (* x 2.0)))))

(defun attack-effectiveness (att def)
  (* 2.0 (smoothstep (- att def) -100 100)))

(defun level-scale (level)
  "Returns a level-based scale factor used when computing health and damage."
  (+ 0.75 (* level 0.25)))

(defun damage-range (actor attack)
  "Returns two values containing the minimum and maximum damage done by `actor'
with `attack'."
  (with-attributes (speed level base-damage damage-variance) attack
    (let* ((k (* base-damage damage-variance))
           (level (or level (? actor :level)))
           (scale (* speed (level-scale level))))
      (values (* scale (- base-damage k))
              (* scale (+ base-damage k))))))

(defun roll-damage (actor attack)
  (multiple-value-call #'random-float (damage-range actor attack)))

(defun resolve-attack (actor attack target)
  "Computes the damage done by an instance of `actor' using `attack' against
`target'. The secondary return value is true if the attack was a critical hit."
  (let* ((att (attack-rating actor attack))
         (def (defense-rating target attack))
         (eff (attack-effectiveness att def))
         (damage (* eff (roll-damage actor attack)))
         (crit-chance (+ 0.05 (critical-chance-bonus actor))))
    (if (< (random 1.0) crit-chance)
        (values (round-random (* damage (+ 1.5 (critical-damage-bonus actor)))) t)
        (values (round-random damage) nil))))

;;; Health.

(defun base-health (combatant)
  (or (? combatant :race :base-health) (? combatant :base-health) 1))

(defun max-health (combatant)
  (floor (* (level-scale (? combatant :level))
            (base-health combatant)
            (1+ (health-bonus combatant)))))

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
  (with-attributes (health max-health) actor
    (when (< health max-health)
      (setf health (min max-health (+ health (base-health actor))))
      (for-contents (avatar (location actor) :type 'avatar)
        (when (not (eq avatar actor))
          (update-neighbor avatar actor :health health))))))

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
      (as:remove-event attack-timer))
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
  (format-log :info "attack ~a ~a" actor target)
  (process-simple-event attack (actor target)
      (:observers (cons (location actor) (? (location actor) :contents)))
    (call-next-method)))

(defgeneric describe-attack (observer actor target attack damage crit)
  (:method (observer actor target attack damage crit)))

(defmethod attack ((actor combatant) (target combatant))
  (with-slots (attack-timer current-attack) actor
    (bind ((damage crit (resolve-attack actor current-attack target)))
      (when (> (? target :health) 0)
        (when (<= (decf (? target :health) damage) 0)
          ;; The target dies but not until after this event has been fully
          ;; processed.
          (with-delay (0)
            (kill actor target))))
      (show-message (? (location actor) :contents)
                    (lambda (e) (describe-attack e actor target
                                                 current-attack damage crit))))
    (setf attack-timer nil)
    (begin-attack actor target)))

;;;

(defgeneric spawn-corpse (entity attackers))

(defmethod spawn-corpse ((entity entity) attackers)
  (when (? entity :leaves-corpse)
    (let ((corpse (clone-entity 'corpse)))
      (setf (corpse-entity corpse) entity)
      (enter-world corpse)
      (enter-location corpse (location entity) nil)
      corpse)))

(defmethod spawn-corpse ((combatant combatant) attackers)
  (when-let ((corpse (call-next-method)))
    (setf (corpse-owners corpse) attackers)
    corpse))

;;;

(defmethod kill :around ((actor combatant) (target combatant))
  (process-simple-event kill (actor target)
      (:observers (observer-list* target (location actor) (? (location actor) :contents)))
    (call-next-method)))

(defgeneric describe-death (observer actor target)
  (:method (observer actor target)))

(defgeneric die (combatant))

(defmethod die ((combatant combatant))
  (despawn-entity combatant))

(defmethod kill ((actor combatant) (target combatant))
  (show-message (? (location actor) :contents) #`(describe-death % actor target))
  (spawn-corpse target (list actor)) ; FIXME: anyone who did damage
  (die target))

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
      (as:remove-event attack-timer))
    (when (> (? actor :health) 0)
      (show-message (combatants battle)
                    (format nil "~a has left the battle."
                            (describe-brief actor :capitalize t)))
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
