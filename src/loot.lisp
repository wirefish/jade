(in-package :jade)

(defgeneric loot (actor corpse))

(defmethod loot :around ((avatar avatar) (corpse corpse))
  (with-slots (owners) corpse
    (cond
      ((find avatar owners)
       (call-next-method)
       (deletef owners avatar)
       (unless owners
         (despawn-entity corpse)))
      (t
       (show avatar "You don't have permission to loot ~a."
             (describe-brief corpse :article :definite))))))

(defmethod loot ((avatar avatar) (corpse corpse))
  (if-let ((items (when-let ((loot (? (corpse-entity corpse) :loot)))
                    (remove-if-not (lambda (x) (can-see avatar x)) (funcall loot)))))
    (receive avatar nil items)
    (show avatar "You find nothing of value.")))

(defcommand loot (actor "loot" corpse)
  "Search a corpse for valuables. You can only loot a corpse of a creature that
you helped to kill."
  (when-let ((corpses (match actor corpse :at-least-one
                        (can-see actor (remove-if-not #`(entity-isa % 'corpse)
                                                      (? (location actor) :contents)))
                        :no-tokens t
                        :no-subjects "There are no corpses here to loot."
                        :no-match "There are no corpses here matching ~s.")))
    (dolist (corpse corpses)
      (loot actor corpse))))
