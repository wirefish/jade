(in-package :jade)

;;; A `resource' is a raw material obtained via gathering.

(defentity resource (item)
  (:required-skill nil
   :required-rank 1
   :stackable t))

;;; A `resource-node' is an entity that allows players to gather resources.

(defclass resource-node (entity) ())

(defentity resource-node (&class resource-node)
  (:required-skill nil
   :required-tool-level 1
   :resources nil
   :users nil))

(defmethod transform-initval ((class (eql 'resource-node)) (name (eql :resources)) value)
  "The `:resources' attribute describes a generator used to determine resources
gathered with each attempt."
  (make-generator value))

;;;

(defgeneric gather (actor node))

(defmethod gather :around (actor node)
  (process-simple-event gather (actor node)
      (:observers (? (location actor) :contents))
    (call-next-method)))

(defmethod gather ((avatar avatar) (node resource-node))
  (let* ((rank (skill-rank avatar (? node :required-skill) 0))
         (resources (? node :resources))
         (obtained (remove-if-not
                    (lambda (resource) (<= (? resource :required-rank) rank))
                    (and resources (funcall resources)))))
    (if obtained
        (progn
          ;; TODO: increase rank based on items received?
          (receive avatar nil obtained))
        (show avatar "You do not obtain anything from ~a."
              (describe-brief node :article :definite)))
    (push avatar (? node :users))
    ;; TODO: make the node disappear after a bit
    ))

;;;

(defcommand gather (actor "gather" "from" source)
  "Gather resources from a nearby source. In order to succeed you must know the
associated gathering skill at the required rank and have an appropriate
gathering tool equipped."
  (let* ((nodes (remove-if-not (lambda (x) (typep x 'resource-node))
                               (? (location actor) :contents)))
         (nodes (if source (find-matches source nodes) nodes)))
    (case (length nodes)
      (0
       (show actor "You don't see anything to gather from~@[ that matches ~s~]."
             (and source (join-tokens source))))
      (1
       (let* ((node (first nodes))
              (skill (symbol-value-as 'skill (? node :required-skill) nil))
              (tool (? actor :equipment :tool)))
         (cond
           ((null skill)
            (format-log :warning "~s requires invalid skill ~s"
                        (entity-type node) (? node :required-skill))
            (show actor "You cannot gather from ~a."
                  (describe-brief node :article :definite)))
           ((null (skill-rank actor (skill-label skill)))
            (show actor "You need to learn the skill ~s before gathering from ~a."
                  (skill-name skill) (describe-brief node :article :definite)))
           ((or (null tool) (not (entity-isa tool (skill-required-tool skill))))
            (show actor "You do not have the right type of tool equipped to gather from ~a."
                  (describe-brief node :article :definite)))
           ((< (? tool :level) (? node :required-tool-level))
            (show actor "Your ~a is not high enough level to use on ~a."
                  (describe-brief tool :article nil)
                  (describe-brief node :article :definite)))
           (t
            (gather actor node)))))
      (t
       (show actor "Do you want to gather from ~a?"
             (format-list #'describe-brief nodes))))))
