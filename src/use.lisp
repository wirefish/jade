(in-package :jade)

(defgeneric use (actor item target))

(defmethod use :around (actor item target)
  (process-simple-event use (actor item target)
      (:observers (list actor item target))
    (call-next-method)))

(defmethod use (actor item target)
  (unless (nth-value 1 (observe-event item :when-use actor item target))
    (show actor "You cannot use ~a." (describe-brief item :article :definite))))


;;;

(defmethod use :around ((avatar avatar) (corpse corpse) target)
  (with-slots (owners) corpse
    (cond
      (target
       (show avatar "You can't use ~a on ~a."
             (describe-brief corpse :article :definite)
             (describe-brief target :article :definite)))
      ((find avatar owners)
       (call-next-method)
       (deletef owners avatar)
       (unless owners
         (despawn-entity corpse)))
      (t
       (show avatar "You don't have permission to loot ~a."
             (describe-brief corpse :article :definite))))))

(defmethod use ((avatar avatar) (corpse corpse) target)
  (with-slots (loot) corpse
    (if-let ((items (remove-if-not (lambda (x) (can-see avatar x)) loot)))
      (receive avatar nil items)
      (show avatar "You find nothing of value.")))
  )

;;
(defcommand use (actor "use" item "on" target)
  "Use an item in your inventory or environment."
  ;; FIXME: inventory, target
  (if item
      (let ((matches (find-matches item (? (location actor) :contents))))
        (case (length matches)
          (0 (show actor "You don't see any items here that match ~s."
                   (join-tokens item)))
          (1 (use actor (first matches) target))
          (t (show actor "Do you want to use ~a?"
                   (format-list #'describe-brief matches "or")))))
      (show actor "What do you want to use?")))
