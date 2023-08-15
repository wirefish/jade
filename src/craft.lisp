(in-package :jade)

#|

The crafting system is meant to allow for creating highly custom items by
varying the specific parts used, constrained by the item's requirements.

For example, a generic dagger might have these requirements:

- 2 metal ingots for the blade;
- 1 piece of leather for the grip; and
- 1 optional gemstone, for the pommel.

The resulting dagger derives its traits, durability, and level from the
ingredients.

|#

(defun compute-item-level (parts)
  (loop for item in parts
        sum (? item :quantity) into quantity
        sum (* (? item :quantity) (? item :level)) into level
        finally (return (floor (/ level quantity)))))

(defun match-part (req part)
  (destructuring-bind (proto &optional material) (ensure-list req)
    (and (entity-isa part proto)
         (or (null material)
             (find material (? part :materials))))))

(defun match-parts (item parts)
  "Returns true if the given parts match the requirements for crafting the given
item. The parts must be in the same order as the requirements are specified."
  (let ((required-parts (? item :craft-parts)))
    ;; TODO: handle optional parts.
    (and (= (length parts) (length required-parts))
         (every #'match-part
                required-parts
                parts))))

(defun part-materials (parts)
  (delete-duplicates (loop for part in parts nconc (? part :materials))))

(defun create-item (proto level parts)
  (clone-entity proto
                :level level
                :traits nil ; TODO: derive from part materials, scale to 1.
                :materials (part-materials parts)))

(defun craft-item (avatar proto parts)
  ;; Check that the parts are appropriate.
  (unless (match-parts proto parts)
    (show avatar
          "You cannot craft ~a using ~a."
          (describe-brief proto)
          (format-list #'describe-brief parts))
    (return-from craft-item))
  ;; Check that the avatar actually has the parts.
  (unless (every (lambda (e)
                   (find-item avatar :inventory e (? e :quantity)))
                 parts)
    (show avatar
          "You do not have ~a."
          (format-list #'describe-brief parts))
    (return-from craft-item))
  (let ((item-level (compute-item-level parts)))
    ;; TODO: Check the resulting level is high enough for proto, and that
    ;; the avatar has the required rank in the required skill.
    ;; TODO: Remove items.
    (let ((item (clone-entity proto
                              :level item-level
                              :traits nil ; TODO: derive from part materials, scale to 1.
                              :materials (part-materials parts)))
          (removed-parts (mapcar (lambda (e) (remove-item avatar :inventory e)) parts)))
      (insert-item avatar :inventory item)
      (update-inventory avatar (list item) removed-parts)
      (show avatar "You craft ~a." (describe-brief item))
      item)))
