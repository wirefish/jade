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
    :in (make-dir "in" nil :out '(0 0 0) (ash 1 10))
    :out (make-dir "out" nil :in '(0 0 0) (ash 1 11))))

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
  (format nil "~a leads ~a."
          (if-let ((portal (exit-portal exit)))
            (describe-brief portal :article :definite :capitalize t)
            "The exit")
          (direction-name (exit-dir exit))))

(defmethod observe-event ((observer exit) event &rest args)
  (apply #'observe-event (exit-portal observer) event args))

(defmethod match-subject (tokens (subject exit))
  (with-slots (dir portal) subject
    (best-match-quality (match-subject tokens (direction-name dir))
                        (when-let ((abbrev (direction-abbrev dir)))
                          (match-subject tokens abbrev))
                        (match-subject tokens portal))))

;;; An entity that acts as a portal may define several attributes used to
;;; construct messages seen by observers when some entity passes through the
;;; portal: :exit-verb, :entry-verb, and :transit-message.

(defun exit-message (actor exit)
  (action-message actor
                  (if exit
                      (with-slots (dir portal) exit
                        (format nil
                                (or (? portal :exit-verb) "heads ~a.")
                                (direction-name dir)))
                      "disappears!")))

(defun entry-message (actor entry)
  (action-message actor
                  (if entry
                      (with-slots (dir portal) entry
                        (format nil
                                (or (? portal :entry-verb)
                                    (case dir
                                      (:up "arrives from above.")
                                      (:down "arrives from below.")
                                      (t "arrives from the ~a.")))
                                (direction-name dir)))
                      "appears!")))

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
    (enter-world entity)))

(defmethod exit-world ((location location))
  (dolist (entity (? location :contents))
    (exit-world entity))
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

(defmethod transform-initval ((name (eql :contents)) value)
  `(list ,@(loop for spec in value
                 collect (let ((proto (if (listp spec) (first spec) spec))
                               (args (when (listp spec) (rest spec))))
                           `(clone-entity ',proto ,@args)))))

(defmethod transform-initval ((name (eql :exits)) value)
  (labels ((transform-exit-group (group)
             (bind (((portal-spec &rest dirs-dests) group)
                    (portal-name (if (listp portal-spec) (first portal-spec) portal-spec))
                    (portal-args (when (listp portal-spec) (rest portal-spec))))
               (loop for (dir dest) on dirs-dests by #'cddr
                     collect `(register-exit ,dir ',dest ',portal-name ,@portal-args)))))
    `(list ,@(loop for group in value
                   nconc (transform-exit-group group)))))

;;;

(defun location (entity)
  "Returns the entity's container if it is a location, or nil otherwise."
  (let ((c (entity-container entity)))
    (when (typep c 'location) c)))

(defun spawn-entity (location &rest args)
  (let ((entity (apply #'clone-entity args)))
    (enter-world entity)
    (enter-location entity location nil)))

(defun spawn-unique-entity (location proto-label &rest attributes)
  (unless (contains-isa location :contents proto-label)
     (apply #'spawn-entity location proto-label attributes)))
