(in-package :jade)

(defgeneric use (actor item target))

(defmethod use :around (actor item target)
  (process-simple-event use (actor item target)
      (:observers (list actor item target))
    (call-next-method)))

(defmethod use (actor item target)
  (notify-observers (list actor item target)
                    :when-use actor item target))

(defcommand use (actor "use" item "on" target)
  "Use an item in your inventory or environment."
  ;; FIXME: inventory
  (if item
      (let ((matches (find-matches-if (lambda (e) (reacts-to-event-p e :when-use))
                                      item
                                      (? (location actor) :contents))))
        (case (length matches)
          (0 (show actor "You don't see any usable items here that match \"~a\"."
                   (join-tokens item)))
          (1 (let ((target nil)) ;; FIXME:
               (when (eq (use actor (first matches) target) :call-next-handler)
                 (show actor "Nothing happens."))))
          (t (show actor "Do you want to use ~a?"
                   (format-list #'describe-brief matches "or")))))
      (show actor "What do you want to use?")))
