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

(defun register-exit (dir dest portal-label &rest portal-args)
  ;; FIXME: check for a matching exit; if found, pop it from the registry and
  ;; share its portal. Otherwise, create a new exit and register it to be found
  ;; later.
  (let ((portal (find-entity portal-label)))
    (make-exit :dir dir :dest dest :portal (apply #'clone-entity portal portal-args))))

(defun describe-exit (exit)
  (format nil "~a leads ~a."
          (if-let ((portal (exit-portal exit)))
            (describe-brief portal :article :definite :capitalize t)
            "The exit")
          (direction-name (exit-dir exit))))

(defmethod observe-event ((observer exit) event &rest args)
  (apply #'observe-event (exit-portal observer) event args))

;;; Use `deflocation' to create an actual location the world.

(defparameter *locations* (make-hash-table)
  "A table of all locations defined with `deflocation'. This is used to facilitate
starting and stopping simulation.")

(defun find-location (name)
  (gethash name *locations*))

(defmacro deflocation (name (&optional proto) attributes &body behaviors)
  (with-gensyms (loc)
    `(let ((,loc (defentity ,name (,proto) ,attributes ,@behaviors)))
       (sethash ',name *locations* ,loc)
       ,loc)))

(defmethod transform-initval ((name (eql :exits)) value)
  (labels ((transform-exit-group (group)
             (bind (((portal-spec &rest dirs-dests) group)
                    (portal-name (if (listp portal-spec) (first portal-spec) portal-spec))
                    (portal-args (when (listp portal-spec) (rest portal-spec))))
               (loop for (dir dest) on dirs-dests by #'cddr
                     collect `(register-exit ,dir ',dest ',portal-name ,@portal-args)))))
    `(list ,@(loop for group in value
                   nconc (transform-exit-group group)))))
