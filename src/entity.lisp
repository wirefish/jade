(in-package :jade)

(defvar *entity-id-counter* 0)

(defclass entity ()
  ((id :initarg :id :initform (incf *entity-id-counter*))
   (label :initarg :label :initform nil)
   (proto :initarg :proto :initform nil)
   (ancestry :initarg :ancestry :initform nil)
   (attributes :initarg :attributes :initform (make-hash-table))
   (behaviors :initform nil)))

(defmethod initialize-instance :after ((entity entity) &key)
  (with-slots (label proto ancestry) entity
    (if proto
        (with-slots ((proto-ancestry ancestry)) proto
          (setf ancestry (if label (cons label proto-ancestry) proto-ancestry)))
        (when label (setf ancestry (list label))))))

(defun entity-isa (entity label)
  (numberp (position label (slot-value entity 'ancestry))))

(defvar *attribute-transforms* (make-hash-table :test 'eq))

(defun transform-attributes (&rest keys-values)
  (let ((attributes (make-hash-table)))
    (loop for (key value) on keys-values by #'cddr do
      (sethash key attributes
               (funcall (or (gethash key *attribute-transforms*) #'identity) value)))
    attributes))

(defmacro defentity (name (&optional base-name) attributes &body behaviors)
  `(prog1
     (defparameter ,name
       (make-instance 'entity
                      :label ',name
                      :proto ,base-name
                      :attributes (transform-attributes ,@attributes)))
     (defbehaviors ,name ,@behaviors)
     (export ',name)))

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))
  (with-slots (attributes proto) instance
    (let ((value (gethash slot-name attributes)))
      (or value (when proto (slot-value proto slot-name))))))

(defmethod slot-missing (class (instance entity) slot-name
                         (operation (eql 'setf)) &optional new-value)
  (with-slots (attributes) instance
    (sethash slot-name attributes new-value)))

(defmacro with-attributes ((&rest names) entity &body body)
  (let ((all-names (remove '&optional names))
        (required-names (subseq names 0 (position '&optional names))))
    `(with-slots (,@all-names) ,entity
       (when (and ,@required-names)
         ,@body))))

;;; Behaviors.

(defstruct behavior constraints fn)

(defun push-behavior (actor action constraints fn)
  (with-slots (behaviors) actor
    (when (null behaviors)
      (setf behaviors (make-hash-table)))
    (push (make-behavior :constraints constraints :fn fn) (gethash action behaviors))))

(defmacro defbehaviors (actor &body clauses)
  (labels ((second-or-nil (x) (when (listp x) (second x)))
           (first-or-self (x) (if (listp x) (first x) x)))
    `(progn
       ,@(loop for (action spec . body) in (reverse clauses)
               collect (let ((constraints (mapcar #'second-or-nil spec))
                             (params (mapcar #'first-or-self spec)))
                         `(push-behavior ,actor ',action ',constraints
                                         (lambda (self ,@params)
                                           (declare (ignorable self ,@params))
                                           ,@body))))
       ,actor)))

#|

The flow when running behaviors is:

1. Start with the observer, the action, and the arguments to that action.

2. Get a list of all applicable behaviors for the action in the order they
should be applied: (? observer 'behaviors action).

3. Get the tail of the list whose car is the first behavior that matches the
arguments.

4. Call the behavior function. If it returns :continue, remove it from the list
and return to step 3.

5. Stop when the list is empty.

6. If the last function called returned :disallow, then do not proceed with the
action. This applies only in the before phase.

|#

(defun everyp (pred &rest lists)
  (if (position-if #'null lists)
      t
      (when-let ((x (apply pred (mapcar #'car lists))))
        (apply #'everyp pred (mapcar #'cdr lists)))))

(defun match-constraints (constraints args)
  (labels ((match-constraint (constraint arg)
             (typecase constraint
               (null t)
               (symbol (and (typep arg 'entity)
                            (entity-isa arg constraint)))
               (otherwise nil))))
    (when (= (length constraints) (length args))
      (everyp #'match-constraint constraints args))))

(defun run-behaviors (observer action &rest args)
  (let ((behaviors (? observer 'behaviors action)))
    (loop for behavior in behaviors do
      (when (match-constraints (behavior-constraints behavior) args)
        (unless (eql (apply #'behavior-fn observer args) :continue)
          (return))))))

;;; Standard attribute transforms.

(sethash :brief *attribute-transforms* #'parse-noun)

;;; TEST:

(defentity foo ()
  (:name "Bob"
   :brief "a human"
   :age 33)

  (:before-enter (actor location entry)
    (print 123)))
