(in-package :jade)

(defgeneric use (actor item target))

(defmethod use :around (actor item target)
  (process-simple-event use (actor item target)
      (:observers (list actor item target))
    (call-next-method)))

(defmethod use (actor item target)
  (unless (nth-value 1 (observe-event item :when-use actor item target))
    (show actor "You cannot use ~a." (describe-brief item :article :definite))))

(defcommand use (actor "use" item "on" target)
  "Use an item in your inventory or environment."
  ;; FIXME: inventory, target
  (if item
      (let* ((candidates (can-see actor (remove actor (? (location actor) :contents))))
             (matches (find-matches item candidates)))
        (case (length matches)
          (0 (show actor "You don't see any items here that match ~s."
                   (join-tokens item)))
          (1 (use actor (first matches) target))
          (t (show actor "Do you want to use ~a?"
                   (format-list #'describe-brief matches "or")))))
      (show actor "What do you want to use?")))
