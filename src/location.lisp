(in-package :jade)

;;; Standard directions of movement.

(defstruct (dir (:constructor make-dir (name abbrev opposite offset map-bit)))
  name abbrev opposite offset map-bit)

(defparameter *directions* #h(
    :north (make-dir "north" "n" :south '(0 -1 0) (ash 1 0))
    :northeast (make-dir "northeast" "ne" :southwest '(1 -1 0) (ash 1 1))
    :east (make-dir "east" "e" :west '(1 0 0) (ash 1 2))
    :southeast (make-dir "southeast" "se" :northwest '(1 1 0) (ash 1 3))
    :south (make-dir "south" "s" :north '(0 1 0) (ash 1 4))
    :southwest (make-dir "southwest" "sw" :northeast '(-1 1 0) (ash 1 5))
    :west (make-dir "west" "w" :east '(-1 0 0) (ash 1 6))
    :northwest (make-dir "northwest" "nw" :southeast '(-1 -1 0) (ash 1 7))
    :up (make-dir "up" "u" :down '(0 0 1) (ash 1 8))
    :down (make-dir "down" "d" :up '(0 0 -1) (ash 1 9))
    :in (make-dir "in" "enter" :out '(0 0 0) (ash 1 10))
    :out (make-dir "out" "exit" :in '(0 0 0) (ash 1 11))))

(defun direction-name (dir)
  (dir-name (gethash dir *directions*)))

(defun direction-abbrev (dir)
  (dir-abbrev (gethash dir *directions*)))

(defun direction-opposite (dir)
  (dir-opposite (gethash dir *directions*)))

(defun direction-offset (dir)
  (dir-offset (gethash dir *directions*)))

(defun direction-map-bit (dir)
  (dir-map-bit (gethash dir *directions*)))

;;; An `exit' represents a one-way connection between locations. It can be
;;; associated with an entity that represents the portal (e.g. a door or tunnel)
;;; between the locations; the portal entity is shared by both corresponding
;;; exits.

(defstruct exit dir dest portal)

(defun register-exit (dir dest portal &rest portal-args)
  ;; FIXME: check for a matching exit; if found, pop it from the registry and
  ;; share its portal. Otherwise, create a new exit and register it to be found
  ;; later.
  (make-exit :dir dir :dest dest :portal (apply #'clone-entity portal portal-args)))

(defmethod describe-full ((exit exit))
  (with-slots (portal dir) exit
    (format nil
            (or (? portal :description) "~a leads ~a.")
            (if portal
                (let ((article (or (? portal :look-article) :indefinite)))
                  (describe-brief portal :article article :capitalize t))
                "An exit")
            (direction-name dir))))

(defmethod observe-event ((observer exit) event &rest args)
  (apply #'observe-event (exit-portal observer) event args))

(defmethod match-subject (tokens (exit exit))
  (with-slots (dir portal) exit
    (best-match-quality (match-subject tokens (direction-name dir))
                        (when-let ((abbrev (direction-abbrev dir)))
                          (match-subject tokens abbrev))
                        (match-subject tokens portal))))

(defmethod can-see (actor (exit exit))
  (when-let ((portal (exit-portal exit)))
    (can-see actor portal)))

;;; Generate messages seen by the actor and observers when an actor enters or
;;; exits a location. If a portal or actor specifies :entry-message or
;;; :exit-message, it will be used as a control-string for a call to format with
;;; two arguments: the direction name and the brief portal description.

(defun exit-message (actor exit)
  (let* ((portal (and exit (exit-portal exit)))
         (template (or (? portal :exit-message)
                       (? actor :exit-message)
                       (if exit "heads ~a." "disappears!"))))
    (make-message actor template
                  (when exit (string-downcase (symbol-name (exit-dir exit))))
                  (when portal (describe-brief portal :article :definite)))))

(defun entry-message (actor entry)
  (let* ((portal (and entry (exit-portal entry)))
         (template (or (? portal :entry-message)
                       (? actor :entry-message)
                       (if entry
                           (case (exit-dir entry)
                             (:up "arrives from above.")
                             (:down "arrives from below.")
                             (t "arrives from the ~a."))
                           "appears!"))))
    (make-message actor template
                  (when entry (string-downcase (symbol-name (exit-dir entry))))
                  (when portal (describe-brief portal :article :definite)))))

;;; A region is an entity with the same name as the package in which it is
;;; defined (without the leading "jade."). It describes properties shared by all
;;; locations defined within that package.

(defun location-region (location)
  "Returns the region entity associated with a location, if any."
  (let* ((package (symbol-package (entity-label location)))
         (package-name (package-name package))
         (sep (position #\. package-name :from-end t))
         (region-name (if sep (subseq package-name (1+ sep)) package-name)))
    (symbol-value-or-nil (intern region-name package))))

;;; A location is a subclass of entity only so it can be used to specialize
;;; generic functions.

(defclass location (entity) ())

(defmethod enter-world ((location location))
  (call-next-method)
  (dolist (entity (? location :contents))
    (setf (entity-container entity) location)
    (enter-world entity)
    (observe-event entity :after-enter-location entity location nil)))

(defmethod exit-world ((location location))
  (dolist (entity (? location :contents))
    (observe-event entity :before-exit-location entity location nil)
    (exit-world entity)
    (setf (entity-container entity) nil))
  (call-next-method))

;;; A prototype for locations.

(defentity location (&class location) ())

;;; Use `deflocation' to create an actual location in the world.

(defvar *locations* (make-hash-table)
  "A table of all locations defined with `deflocation'. This is used to facilitate
starting and stopping simulation.")

(defun find-location (name)
  (gethash name *locations*))

(defmacro deflocation (name (&optional (proto 'location)) attributes &body behaviors)
  (with-gensyms (loc)
    `(let ((,loc (defentity ,name (,proto) ,attributes ,@behaviors)))
       (assert (typep ,loc 'location))
       (setf (? ,loc :region) (location-region ,loc))
       (sethash ',name *locations* ,loc)
       ,loc)))

(defmethod transform-initval (class (name (eql :contents)) value)
  (loop for spec in value
        collect (apply #'clone-entity (ensure-list spec))))

(defmethod transform-initval (class (name (eql :exits)) value)
  (labels ((transform-exit-group (group)
             (bind (((portal-spec &rest dirs-dests) group))
               (loop for (dir dest) on dirs-dests by #'cddr
                     collect (apply #'register-exit dir dest (ensure-list portal-spec))))))
    (loop for group in value
          nconc (transform-exit-group group))))

;;;

(defun location (entity)
  "Returns the entity's container if it is a location, or nil otherwise."
  (let ((c (entity-container entity)))
    (when (typep c 'location) c)))

(defun find-exit (location dir)
  "Returns the exit in `location' with direction `dir', if any."
  (find-if (lambda (exit) (eq (exit-dir exit) dir))
           (? location :exits)))

;;;

(defun spawn-entity (location label &rest args)
  "Creates a new entity by calling clone-entity with `label' and `args' and
places the entity in `location'."
  (let* ((proto (symbol-value-as 'entity label))
         (allocator (? proto :allocator)))
    (when (or (null allocator) (funcall allocator :acquire))
      (let ((entity (apply #'clone-entity proto args)))
        (enter-world entity)
        (enter-location entity location nil)
        entity))))

(defun spawn-unique-entity (location label &rest attributes)
  "Like spawn-entity, but will not create an entity if a similar one already
exists in `location'."
  (unless (contains-isa location :contents label)
    (apply #'spawn-entity location label attributes)))

(defun limit-spawn-quantity (label quantity)
  "Assigns a limit to the number of entities with prototype `label' that
spawn-entity can cause to exist at the same time."
  (when-let ((entity (symbol-value-as 'entity label)))
    (setf (? entity :allocator) (make-allocator quantity))))

(defun despawn-entity (entity)
  (exit-location entity (location entity) nil :force t)
  (exit-world entity))

(defun respawn-entity (entity location)
  (exit-location entity (location entity) nil :force t)
  (enter-location entity location nil))
