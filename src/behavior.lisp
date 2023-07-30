(in-package :jade)

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
