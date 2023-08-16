(in-package :jade)

;;; A `resource' is a raw material obtained via gathering.

(defentity resource (item)
  (:required-skill nil
   :required-rank 1
   :stackable t))

;;; A `resource-node' is an entity that allows players to gather resources. Its
;;; `:resource' attribute is a list of lists, each of the following form:
;;; (resource-label probability min-obtained max-obtained)

(defentity resource-node ()
  (:required-skill nil
   :resources nil
   :recent-users nil))

;;;

(defgeneric gather (actor node))

(defmethod gather :around (actor node)
  (process-simple-event gather (actor node)
      (:observers (? (location actor) :contents))
    (call-next-method)))

(defmethod gather (actor node)
  (let ((obtained (loop for (resource probability min max) in (? node :resources)
                        when (< (random 1.0) probability)
                          collect (clone-entity resource :quantity (random-int min max)))))
    (receive actor nil obtained)
    (push (cons (avatar-id actor) (get-universal-time)) (? node :recent-users))))

(defun required-gathering-rank (node)
  (loop for (label probability) in (? node :resources)
        maximize (? (symbol-value label) :required-rank)))

;;;

(defcommand gather (actor "gather" "from" source)
  "Gather resources from a nearby source. In order to succeed you must know the
associated gathering skill at the required rank and have an appropriate
gathering tool equipped."
  (let* ((nodes (remove-if-not (lambda (x) (? x :resources))
                               (? (location actor) :contents)))
         (nodes (if source (find-matches source nodes) nodes)))
    (case (length nodes)
      (0
       (show actor "You don't see anything to gather from~@[ that matches ~s~]."
             (and source (join-tokens source))))
      (1
       (let* ((node (first nodes))
              (skill (symbol-value (? node :required-skill)))
              (rank (gethash (skills actor) (skill-label skill))))
         (cond
           ((null rank)
            (show actor "You need to learn the skill ~s before gathering from ~a."
                  (skill-name skill) (describe-brief node :article :definite)))
           ((< rank (required-gathering-rank node))
            (show actor "Your rank in the skill ~s is too low to gather from ~a."
                  (skill-name skill) (describe-brief node :article :definite)))
           (t
            (gather actor node)))))
      (t
       (show actor "Do you want to gather from ~a?"
             (format-list #'describe-brief nodes))))))
