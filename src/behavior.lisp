;;;; Facilities to implement an entity's behavior, which determines how it
;;;; reacts to events that occur in the game world.

(in-package :jade)

;;; Some terminology and an overview:
;;;
;;; An `action' is something that can occur in the world. Some example actions
;;; might be entering a location, opening a box, or casting a spell.
;;;
;;; In the context of an action, the `actor' is the entity that performs the
;;; action. The `participants' are any entities that are directly involved with
;;; the action, including the actor. The `observers' are all entities that can
;;; witness and react to the action.
;;;
;;; Every action is broken down into four `phases'. Each phase triggers a
;;; corresponding `event'. The phases are `allow', `before', `do', and `after'.
;;; Events are named by keywords that combine the names of the phase and action,
;;; e.g. :before-enter-location.
;;;
;;; For each phase, all observers (or participants in the case of the do phase)
;;; have the opportunity to react to the event. This is done by defining an
;;; appropriate `event-handler' within `defentity' or `defbehavior'.
;;;
;;; An entity's `behavior' is simply an ordered collection of event handlers.
;;;
;;; The event phases work as follows:
;;;
;;; * allow: In this phase a handler can call `disallow-action' to prevent the
;;;   action from occuring. Most, but not all, actions include this phase.
;;;
;;; * before: Handlers in this phase are called before the action actually
;;;   happens, but after it is known that it will happen.
;;;
;;; * do: Handlers in this phase are called only on the particpants, and should
;;;   make any state changes necessary to implement the action.
;;;
;;; * after: Handlers in this phase are called after the action has completed.
;;;
;;; In all phases, a handler can call `call-next-handler' to pass control to the
;;; next matching handler.

(defstruct event-handler
  "Combines a `test' function that checks if this handler applies to specific
event arguments, and a `fn' function that implements the response to the event."
  test fn)

(defun push-event-handler (entity event test fn)
  "Adds an event handler to the front of the list of an entity's handlers for a
specific event."
  (with-slots (behavior) entity
    (when (null behavior)
      (setf behavior (make-hash-table)))
    (push (make-event-handler :test test :fn fn) (gethash event behavior))))

(defun allow-phase-p (event)
  "Returns t if `event' names an event in the allow phase."
  (string-starts-with (symbol-name event) "ALLOW-"))

(defun make-event-handler-fn (event params body)
  "Returns a lambda expression that implements the body of an event handler."
  `(lambda (self ,@params)
     (declare (ignorable self ,@params))
     (block handler-body
       (labels (,@(when (tree-contains 'call-next-handler body)
                    '((call-next-handler () (return-from handler-body :call-next-handler))))
                ,@(when (and (allow-phase-p event)
                             (tree-contains 'disallow-action body))
                    '((disallow-action () (return-from handler-body :disallow-action)))))
         ,@body))))

(defun constraint-test (param constraint)
  "Returns an expression that evaluates to true if the value of `param'
satisfies `constraint'."
  (if (symbolp constraint)
      `(entity-isa ,param ',constraint)
      (destructuring-bind (fn . args) constraint
        `(,fn ,param ,@args))))

(defun make-event-handler-test (params constraints)
  "Returns a lambda expression that implements the test function for an event
handler."
  (let ((tests (loop for param in params for constraint in constraints when constraint
                     collect (constraint-test param constraint))))
    `(lambda ,params
       (declare (ignorable ,@params))
       ,(if tests `(and ,@tests) t))))

(defmacro defbehavior (entity &body clauses)
  (labels ((second-or-nil (x) (when (listp x) (second x)))
           (first-or-self (x) (if (listp x) (first x) x)))
    `(progn
       ,@(loop for (event spec . body) in (reverse clauses)
               collect (let ((constraints (mapcar #'second-or-nil spec))
                             (params (mapcar #'first-or-self spec)))
                         `(push-event-handler
                           ,entity ',event
                           ,(make-event-handler-test params constraints)
                           ,(make-event-handler-fn event params body))))
       ,entity)))

(defgeneric observe-event (observer event &rest args)
  (:method ((observer null) event &rest args)
    (declare (ignore args)))
  (:method ((observer entity) event &rest args)
    (let ((handlers (? observer 'behavior event)))
      (loop for handler in handlers do
        (when (apply (event-handler-test handler) args)
          (let ((x (apply (event-handler-fn handler) observer args)))
            (unless (eq x :call-next-handler)
              (return-from observe-event x))))))))

(defun notify-observers (observers event &rest args)
  (dolist (observer observers)
    (apply #'observe-event observer event args)))

(defun observers-allow-p (observers event &rest args)
  (dolist (observer observers)
    (when (eq (apply #'observe-event observer event args) :disallow-action)
      (return-from observers-allow-p nil)))
  t)
