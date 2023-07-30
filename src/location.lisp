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

(defun register-exit (dir dest portal-proto &rest portal-args)
  ;; FIXME: check for a matching exit; if found, pop it from the registry and
  ;; share its portal. Otherwise, create a new exit and register it to be found
  ;; later.
  (make-exit :dir dir :dest dest :portal (apply #'clone-entity portal-proto portal-args)))

;;; Use `deflocation' to create an actual location the world.

(defparameter *locations* (make-hash-table)
  "A table of all locations defined with `deflocation'.")

(defmacro deflocation (name (&optional proto) attributes &body behaviors)
  `(progn
     (defentity ,name (,proto) ,attributes ,@behaviors)
     (sethash ',name *locations* ,name)))

(defmethod transform-initval (type (name (eql :exits)) value)
  `(list ,@(loop for (portal-spec . dirs-dests) in value
                 nconc (loop for (dir dest) on dirs-dests by #'cddr
                             collect `(register-exit ,dir ',dest
                                                     ,@(if (listp portal-spec) portal-spec
                                                           (list portal-spec)))))))

(deflocation foyer ()
  (:name "Foyer"
   :brief "a foyer"
   :exits ((tunnel :west kitchen :east library)
            ((tunnel :color :blue) :south greathall))))
