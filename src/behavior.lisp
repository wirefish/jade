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

(defun make-constraint-test (observer param constraint)
  "Returns an expression that evaluates to true if the value of `param'
satisfies `constraint'."
  (cond
    ((eq (first constraint) 'self)
     `(eq ,param ,observer))
    ((eq (first constraint) '&quest)
     (destructuring-bind (quest &optional phase) (rest constraint)
       (if phase
           `(eq (quest-phase ,param ',quest :as-label t) ',phase)
           `(eq (quest-label ,param) ',quest))))
    ((symbolp (first constraint))
     `(entity-isa ,param ',(first constraint)))
    (t
     (error "invalid constraint ~a" constraint))))

(defun make-event-handler-test (params constraints)
  "Returns a lambda expression that implements the test function for an event
handler."
  (let* ((observer (gensym))
         (tests (loop for param in params for constraint in constraints when constraint
                     collect (make-constraint-test observer param constraint))))
    `(lambda (,observer ,@params)
       (declare (ignorable ,observer ,@params))
       ,(if tests `(and ,@tests) t))))

(defun anonymize-parameters (param-specs)
  (loop for spec in param-specs
        collect (if (eq spec 'self)
                    (list (gensym) 'self)
                    spec)))

(defmacro defbehavior (entity &body clauses)
  (labels ((rest-or-nil (x) (when (listp x) (rest x)))
           (first-or-self (x) (if (listp x) (first x) x)))
    `(progn
       ,@(loop for (event spec . body) in (reverse clauses)
               collect (let* ((spec (anonymize-parameters spec))
                              (constraints (mapcar #'rest-or-nil spec))
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
        (when (apply (event-handler-test handler) observer args)
          (let ((x (apply (event-handler-fn handler) observer args)))
            (unless (eq x :call-next-handler)
              (return-from observe-event x))))))))

(defun notify-observers (observers event &rest args)
  (dolist (observer observers)
    (apply #'observe-event observer event args)))

(defun action-message (actor verb)
  (let* ((verb (parse-verb verb))
         (self-message (format nil "You ~a" (verb-plural verb)))
         (other-message (format nil "~a ~a"
                               (describe-brief actor :capitalize t)
                               (verb-singular verb))))
    (lambda (observer)
      (if (eq observer actor) self-message other-message))))

(defun show-observers (observers message event &rest args)
  (dolist (observer observers)
    (show observer (if (functionp message) (funcall message observer) message))
    (apply #'observe-event observer event args)))

(defun observers-allow-p (observers event &rest args)
  (dolist (observer observers)
    (when (eq (apply #'observe-event observer event args) :disallow-action)
      (return-from observers-allow-p nil)))
  t)

(defun reacts-to-event-p (observer event)
  (not (null (? observer 'behavior event))))
