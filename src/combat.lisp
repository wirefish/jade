(in-package :jade)

;;; A combatant represents an entity that can enter combat.

(defclass combatant (entity)
  ((battle :initform nil)
   (current-target :initform nil)
   (attack-timer :initform nil)))

(defentity combatant (&class combatant)
  (:level 1
   :max-health 1
   :attacks nil
   :traits '(:defense 0)
   :attitude :neutral)  ; or :friendly, :hostile

  (:before-enter-world ()
    (setf (? self :health) (? self :max-health))))

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

;;;

(defaction enter-combat (actor target)
    (:observers (? (location actor) :contents))
  (with-slots ((actor-battle battle)) actor
    (with-slots ((target-battle battle)) target
      (cond
        ((and (null actor-battle) (null target-battle))
         ;; Create a new battle between just actor and target.
         (let ((battle (make-instance 'battle :combatants (list actor target))))
           (setf actor-battle battle
                 target-battle battle)
           (show actor "You enter combat against ~a!" (describe-brief target))
           (show target "You enter combat against ~a!" (describe-brief actor))))
        ((and actor-battle target-battle)
         ;; TODO: merge the two battles.
         )
        (t
         ;; TODO: Add actor or target to the other's existing battle.
         )))))

;;;

(defun leave-battle (actor)
  (format-log :info "~a is leaving battle" actor)
  (with-slots (battle current-target attack-timer) actor
    (deletef (combatants battle) actor)
    (when attack-timer
      (cl-async:remove-event attack-timer))
    (let ((msg (format nil "~a has left the battle." (describe-brief actor :capitalize t))))
      (dolist (c (combatants battle))
        (show c msg)))
    (show actor "You are no longer in combat.")
    (setf current-target nil attack-timer nil battle nil)))

(defaction leave-combat (actor battle)
    (:observers (combatants battle))
  (leave-battle actor)
  ;; FIXME: dissolve the battle if all remaining combatants are on the same side.
  (when (= (length (combatants battle)) 1)
    (leave-battle (first (combatants battle)))))

(defgeneric try-leave-combat (actor)
  (:method (actor)
    t)
  (:method ((actor combatant))
    (if-let ((battle (? actor 'battle)))
      (leave-combat actor battle)
      t)))

;;;

(defcommand attack (actor ("attack" "at") target)
  "Enter combat with *target*. By default you will automatically perform basic
attacks with the weapon in your main hand. Type `help combat` for more
information."
  (bind ((candidates (remove actor (? (location actor) :contents)))
         (target quantity (split-quantity target))
         (targets (if target
                      (find-matches target candidates)
                      (delete-if-not (lambda (e)
                                       (and (entity-isa e 'combatant)
                                            (not (entity-isa e 'avatar))))
                                     candidates))))
    (cond
      ((null targets)
       (show actor (if target
                       (format nil "You don't see anything matching \"~a\" to attack."
                               (join-tokens target))
                       "You don't see anything to attack.")))
      ((or (= (length targets) 1) (eql quantity 1))
       (enter-combat actor (first targets)))
      (t (show actor "Do you want to attack ~a?"
               (format-list #'describe-brief targets "or"))))))
