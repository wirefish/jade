;;;; An activity represents an action that takes some time to complete.

(in-package :jade)

(defclass activity ()
  ())

(defgeneric cancel-activity (actor activity)
  (:method :around (actor activity)
    (when (eq (entity-activity actor) activity)
      (setf (entity-activity actor) nil)
      (call-next-method))))

(defgeneric begin-activity (actor activity)
  (:method :around (actor activity)
    (when-let ((previous-activity (entity-activity actor)))
      (cancel-activity actor previous-activity))
    (setf (entity-activity actor) activity)
    (call-next-method)))

(defgeneric finish-activity (actor activity)
  (:method :around (actor activity)
    (when (eq (entity-activity actor) activity)
      (setf (entity-activity actor) nil)
      (call-next-method))))

(defun cancel-current-activity (actor)
  (when-let ((activity (entity-activity actor)))
    (cancel-activity actor activity)))
