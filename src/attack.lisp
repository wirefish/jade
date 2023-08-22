(in-package :jade)

(defcommand attack (actor ("attack" "at") target)
  "If already in combat with *target*, make it your current target. Otherwise,
enter combat with *target*. By default you will automatically begin to perform
basic attacks with the weapon in your main hand. Type `help combat` for more
information."
  (bind ((target quantity (split-quantity target)))
    (with-slots (battle current-target) actor
      (if battle
          (let* ((candidates (enemies actor battle))
                 (targets (if target
                              (find-matches target candidates)
                              (or (ensure-list current-target) candidates))))
            (cond
              ((null targets)
               (if current-target
                   (show actor "You are attacking ~a." (describe-brief current-target))
                   (show actor "What do you want to attack?")))
              ((or (= (length targets) 1) (eql quantity 1))
               (if (eql (first targets) current-target)
                   (show actor "You continue attacking ~a." (describe-brief current-target))
                   (progn
                     (setf current-target (first targets))
                     (show actor "You begin attacking ~a." (describe-brief current-target))
                     (begin-attack actor current-target))))))
          (let* ((candidates (can-see actor (remove actor (? (location actor) :contents))))
                 (targets (if target
                              (find-matches target candidates)
                              (delete-if-not (lambda (e)
                                               (and (entity-isa e 'combatant)
                                                    (not (eq (? e :attitude) :friendly))))
                                             candidates))))
            (cond
              ((null targets)
               (show actor (if target
                               (format nil "You don't see anything matching ~s to attack."
                                       (join-tokens target))
                               "You don't see anything to attack.")))
              ((or (= (length targets) 1) (eql quantity 1))
               (enter-combat actor (first targets)))
              (t (show actor "Do you want to attack ~a?"
                       (format-list #'describe-brief targets "or")))))))))
