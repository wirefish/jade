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
;;; corresponding `event'. The phases are `allow', `before', `when', and `after'.
;;; Events are named by keywords that combine the names of the phase and action,
;;; e.g. :before-enter-location.
;;;
;;; For each phase, all observers (or participants in the case of the when
;;; phase) have the opportunity to react to the event. This is done by defining
;;; an appropriate event handler within `defentity' or `defbehavior'.
;;;
;;; An entity's `behavior' is simply an ordered collection of event handlers.
;;; The first handler that matches the event name and arguments is called to
;;; handle the event.
;;;
;;; The event phases work as follows:
;;;
;;; * allow: In this phase a handler can call `disallow-action' to prevent the
;;;   action from occuring. Most, but not all, actions include this phase.
;;;
;;; * before: Handlers in this phase are called before the action actually
;;;   happens, but after it is known that it will happen.
;;;
;;; * when: Handlers in this phase are called only on the particpants, and
;;;   should make any state changes necessary to implement the action.
;;;
;;; * after: Handlers in this phase are called after the action has completed.
;;;
;;; In all phases, a handler can call `call-next-handler' to pass control to the
;;; next matching handler, if any.

(defstruct event-handler event params fn)

(defun push-event-handler (entity event params fn)
  "Adds an event handler to the front of the list of an entity's handlers for a
specific event."
  (with-slots (behavior) entity
    (when (null behavior)
      (setf behavior (make-hash-table)))
    (push (make-event-handler :event event :params params :fn fn)
          (gethash event behavior))))

(defun allow-phase-p (event)
  "Returns t if `event' names an event in the allow phase."
  (string-starts-with (symbol-name event) "ALLOW-"))

(defun make-constraint-test (observer param constraint)
  "Returns an expression that evaluates to true if the value of `param'
satisfies `constraint'."
  (cond
    ((eq (first constraint) nil)
     `(null ,param))
    ((eq (first constraint) 'self)
     `(eq ,param ,observer))
    ((eq (first constraint) '&quest)
     (destructuring-bind (quest &optional phase) (rest constraint)
       (if phase
           `(eq (current-quest-phase ,param ',quest) ',phase)
           `(eq (quest-label ,param) ',quest))))
    ((eq (first constraint) '&race)
     `(eq (? ,param :race 'label) ',(second constraint)))
    ((eq (first constraint) '&dir)
     `(eq (exit-dir ,param) ,(second constraint)))
    ((symbolp (first constraint))
     `(entity-isa ,param ',(first constraint)))
    ((stringp (first constraint))
     `(apply #'match-subjects ,param ',constraint))
    (t
     (error "invalid constraint ~a" constraint))))

(defun make-handler-fn (event params body)
  "Constructs a lambda expression that implements a handler for `event' by
checking any constraints in `params' and executing `body' if all constraints are
satisfied."
  (let ((tests (loop for (name . constraint) in params
                     when constraint
                       collect (make-constraint-test 'self name constraint)))
        (param-names (mapcar #'car params))
        (handler-body (gensym))
        (more-handlers (gensym)))
    `(lambda (self ,more-handlers ,@param-names)
       (declare (ignorable self ,@param-names ,more-handlers))
       (block ,handler-body
         (labels (,@(when (tree-contains 'call-next-handler body)
                      `((call-next-handler () (continue-event self ,more-handlers
                                                              ,event ,@param-names))))
                  ,@(when (and (allow-phase-p event)
                               (tree-contains 'disallow-action body))
                      `((disallow-action () (return-from ,handler-body :disallow-action)))))
           ,@(when tests
               `((unless (and ,@tests) (return-from ,handler-body 'constraints-not-satisfied))))
           ,@body)))))

(defun normalize-parameters (params)
  (loop for param in params
        collect (if (eq param 'self)
                    (list (gensym) 'self)
                    (ensure-list param))))

;;;

(defun actor-quest-constraint (params)
  "When the first (actor) parameter in `params' has a &quest constraint, returns
two values: the quest label and phase specified in the constraint."
  (when-let ((constraint (rest (first params))))
    (when (eq (first constraint) '&quest)
      (values-list (rest constraint)))))

(defun find-calls (fn body)
  "Returns all forms that appear to be a call to the function named `fn' in
`body', in depth-first order."
  (find-all-in-tree-if (lambda (x) (and (listp x) (eq (first x) fn))) body))

(defun cache-offered-quests (entity event params body)
  "Finds all calls to the offer-quest function in `body' and pushes the labels
of the associated quests into the :offers-quests attribute of `entity'. Also
checks that the call is associated with an actor constraint that the quest is
available."
  (declare (ignore event))
  (bind ((actor-quest actor-phase (actor-quest-constraint params)))
    (loop for offer in (find-calls 'offer-quest body)
          do
             (let ((quest (second (third offer))))
               (if (and (eq quest actor-quest) (eq actor-phase :available))
                   (pushnew quest (? entity :offers-quests))
                   (error "cannot offer quest ~a without matching :available constraint"
                          quest))))))

(defun cache-advanced-quests (entity event params body)
  "Finds all calls to the advance-quest function in `body' and pushes lists
of (event quest-label quest-phase) into the :advances-quests attribute of
`entity', where `quest-phase' is obtained from the required matching quest
constraint for the first (actor) parameter in `params'."
  (bind ((actor-quest actor-phase (actor-quest-constraint params)))
    (loop for advance in (find-calls 'advance-quest body)
          do
             (let ((quest (second (fourth advance))))
               (if (eq quest actor-quest)
                   (pushnew (list event quest actor-phase)
                            (? entity :advances-quests) :test #'equal)
                   (error "cannot advance quest ~a without matching actor constraint"
                          quest))))))

(defun postprocess-handler (entity event params body)
  (cache-advanced-quests entity event params body)
  (cache-offered-quests entity event params body))

(defmacro defbehavior (label &body clauses)
  (with-gensyms (entity)
    `(let ((,entity (symbol-value ',label)))
       ,@(loop for (event params . body) in (reverse clauses)
               nconc
               (let ((params (normalize-parameters params)))
                 (list
                  `(push-event-handler
                    ,entity ',event ',params ,(make-handler-fn event params body))
                  `(postprocess-handler ,entity ',event ',params ',body))))
       ,entity)))

;;;

(defgeneric observe-event (observer event &rest args))

(defmethod observe-event (observer event &rest args)
  (declare (ignore args)))

(defmethod continue-event (observer handlers event &rest args)
  (loop for (handler . more-handlers) on handlers do
    (handler-case
        (let ((result (apply (event-handler-fn handler) observer more-handlers args)))
          (unless (eq result 'constraints-not-satisfied)
            (return-from continue-event (values result t))))
      (error (e)
        (format-log :warning "error in event handler ~a ~a ~a: ~a"
                    (entity-type observer)
                    event
                    (event-handler-params handler)
                    e))))
  nil)

(defmethod observe-event ((observer entity) event &rest args)
  (apply #'continue-event observer (? observer 'behavior event) event args))

(defun observers-allow (observers event &rest args)
  (dolist (observer observers)
    (when (eq (apply #'observe-event observer event args) :disallow-action)
      (return-from observers-allow nil)))
  t)

(defun observer-explicity-allows (observer event &rest args)
  (let ((value (apply #'observe-event observer event args)))
    (and value (not (eq value :disallow-action)))))

(defun notify-observers (observers event &rest args)
  (dolist (observer observers)
    (apply #'observe-event observer event args)))

;;;

(defun reacts-to-event-p (observer event)
  (? observer 'behavior event))

(defun reacts-to-quest-phase (observer label phase)
  "Returns true if `observer' has any event handler whose first parameter has a
quest constraint that matches `label' and `phase'."
  (when-let ((behavior (entity-behavior observer)))
    (maphash-values (lambda (handlers)
                      (when (some (lambda (handler)
                                    (let ((param (first (event-handler-params handler))))
                                      (and (eq (second param) '&quest)
                                           (eq (third param) label)
                                           (eq (fourth param) phase))))
                                  handlers)
                        (return-from reacts-to-quest-phase t)))
                    behavior)))

(defun observer-list* (actor &rest objects)
  "Like list* except that `actor' appears only once in the resulting list, and
always as the first element."
  (cons actor (remove actor (apply #'list* objects))))

;;;

(defmacro process-simple-event (name args (&key observers force) &body body)
  "Returns a form that implements the process of triggering the allow, before, and
after phases for an event. The body is executed between the before and after
phases."
  (let* ((name-str (symbol-name name))
         (allow-phase (make-keyword (strcat "ALLOW-" name-str)))
         (before-phase (make-keyword (strcat "BEFORE-" name-str)))
         (after-phase (make-keyword (strcat "AFTER-" name-str)))
         (observers-var (gensym)))
    `(let ((,observers-var ,observers))
       (when (or ,force (observers-allow ,observers-var ,allow-phase ,@args))
         (notify-observers ,observers-var ,before-phase ,@args)
         ,@body
         (notify-observers ,observers-var ,after-phase ,@args)
         t))))
