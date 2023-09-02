(in-package :jade)

(defgeneric gather (actor node))

(defmethod gather ((avatar avatar) (node resource-node))
  (let* ((rank (skill-rank avatar (? node :required-skill) 0))
         (resources (? node :resources))
         (obtained (remove-if-not
                    (lambda (resource) (<= (? resource :required-rank) rank))
                    (and resources (funcall resources)))))
    (if obtained
        (progn
          (receive avatar "You obtain ~a." obtained)
          (with-attributes (required-skill required-rank) node
            (increase-skill-rank avatar required-skill required-rank)))
        (show avatar "You do not obtain anything from ~a."
              (describe-brief node :article :definite)))))

;;;

(defclass gathering (activity)
  ((node :initarg :node :initform nil :reader gathering-node)))

(defparameter *gathering-duration* 3)  ; FIXME: from node?

(defmethod begin-activity (actor (activity gathering))
  (let ((location (location actor)))
    (with-slots (node) activity
      (push actor (resource-node-users node))
      (show-message (? location :contents) actor "begins to gather from ~a."
                    (describe-brief node))
      (notify-observers (list* location (? location :contents))
                        :before-gather actor node)
      (start-casting actor *gathering-duration*)
      (with-delay (*gathering-duration*)
        (finish-activity actor activity)))))

(defmethod finish-activity (actor (activity gathering))
  (let ((location (location actor)))
    (with-slots (node) activity
      (stop-casting actor)
      (gather actor node)
      (show-message (? location :contents) actor "finishes gathering.")
      (notify-observers (list* location (? location :contents))
                        :after-gather actor node)
      (unless (deletef (resource-node-users node) actor)
        (despawn-entity node)))))

(defmethod cancel-activity (actor (activity gathering))
  (stop-casting actor)
  (deletef (resource-node-users (gathering-node activity)) actor)
  (show actor "Your gathering has been interrupted."))

;;;

(defcommand gather (actor "gather" "from" source)
  "Gather resources from a nearby source. In order to succeed you must know the
associated gathering skill at the required rank and have an appropriate
gathering tool equipped."
  (when-let ((node (match actor source :exactly-one
                     (remove-if-not (lambda (x) (typep x 'resource-node))
                                    (? (location actor) :contents))
                     :no-tokens t
                     :no-subjects "You don't see anything here to gather from."
                     :no-match "You don't see anything to gather from that matches ~s."
                     :multi-match "Do you want to gather from ~a?")))
    (let* ((skill (symbol-value-as 'skill (? node :required-skill) nil))
           (rank (skill-rank actor (skill-label skill)))
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
        ((null rank)
         (show actor "You need to learn ~a before gathering from ~a."
               (skill-name skill) (describe-brief node :article :definite)))
        ((< rank (? node :required-rank))
         (show actor "Your rank in ~a is too low to gather from ~a."
               (skill-name skill) (describe-brief node :article :definite)))
        ((or (null tool) (not (entity-isa tool (skill-required-tool skill))))
         (show actor "You do not have the right type of tool equipped to gather from ~a."
               (describe-brief node :article :definite)))
        ((< (? tool :level) (? node :required-tool-level))
         (show actor "Your ~a is not high enough level to use on ~a."
               (describe-brief tool :article nil)
               (describe-brief node :article :definite)))
        (t
         (begin-activity actor (make-instance 'gathering :node node)))))))
