(in-package :jade)

;;; A `resource' is a raw material obtained via gathering.

(defentity resource (item)
  (:required-skill nil
   :required-rank 1
   :stackable t))

;;; A `resource-node' is an entity that allows players to gather resources.

(defclass resource-node (entity)
  ((users :initform nil :accessor resource-node-users)))

(defentity resource-node (&class resource-node)
  (:required-skill nil
   :required-tool-level 1
   :resources nil))

(defmethod transform-initval ((class (eql 'resource-node)) (name (eql :resources)) value)
  "The `:resources' attribute describes a generator used to determine resources
gathered with each attempt."
  (make-generator value))

;;;

(defgeneric gather (actor node))

(defmethod gather ((avatar avatar) (node resource-node))
  (let* ((rank (skill-rank avatar (? node :required-skill) 0))
         (resources (? node :resources))
         (obtained (remove-if-not
                    (lambda (resource) (<= (? resource :required-rank) rank))
                    (and resources (funcall resources)))))
    (if obtained
        (progn
          ;; TODO: increase rank based on items received?
          (receive avatar "obtain" obtained))
        (show avatar "You do not obtain anything from ~a."
              (describe-brief node :article :definite)))))

;;;

(defclass gathering (activity)
  ((node :initarg :node :initform nil :reader gathering-node)))

(defparameter *gathering-duration* 3)  ; FIXME: from node?

(defmethod begin-activity (actor (activity gathering))
  (with-slots (node) activity
    (push actor (resource-node-users node))
    (let ((msg (action-message actor "begins to gather from ~a." (describe-brief node))))
      (show-observers (observer-list* actor (location actor) (? (location actor) :contents))
                      msg :before-gather actor))
    (start-casting actor *gathering-duration*)
    (with-delay (*gathering-duration*)
      (finish-activity actor activity))))

(defmethod finish-activity (actor (activity gathering))
  (with-slots (node) activity
    (stop-casting actor)
    (gather actor node)
    (let ((msg (action-message actor "finishes gathering.")))
      (show-observers (? (location actor) :contents) msg :after-gather actor))
    (unless (deletef (resource-node-users node) actor)
      (despawn-entity node))))

(defmethod cancel-activity (actor (activity gathering))
  (stop-casting actor)
  (deletef (resource-node-users (gathering-node activity)) actor)
  (show actor "Your gathering has been interrupted."))

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
           ((find actor (resource-node-users node))
            (show actor "You cannot gather from ~a again."
                  (describe-brief node :article :definite)))
           ((null (skill-rank actor (skill-label skill)))
            (show actor "You need to learn ~a before gathering from ~a."
                  (skill-name skill) (describe-brief node :article :definite)))
           ((or (null tool) (not (entity-isa tool (skill-required-tool skill))))
            (show actor "You do not have the right type of tool equipped to gather from ~a."
                  (describe-brief node :article :definite)))
           ((< (? tool :level) (? node :required-tool-level))
            (show actor "Your ~a is not high enough level to use on ~a."
                  (describe-brief tool :article nil)
                  (describe-brief node :article :definite)))
           (t
            (begin-activity actor (make-instance 'gathering :node node))))))
      (t
       (show actor "Do you want to gather from ~a?"
             (format-list #'describe-brief nodes))))))
