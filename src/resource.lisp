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
   :required-rank 1
   :required-tool-level 1
   :resources nil
   :entry-message "catches your attention."
   :exit-message "is depleted."))

(defmethod transform-initval ((class (eql 'resource-node)) (name (eql :resources)) value)
  "The `:resources' attribute describes a generator used to determine resources
gathered with each attempt."
  (make-generator value))
