(in-package :jade)

(defun walk-map (origin radius &key observer up-down cross-domains)
  "Returns a list that contains, for every location reachable from `origin' within
`radius' steps, a list (dx dy dz location) where `dx', `dy', and `dz' are the
position of `location' relative to `origin'. If `observer' is non-nil, only
exits visible to that entity are traversed. Up and down exits are considered
only if `up-down' is t. Locations with a different domain than `origin' are
visited only if `cross-domains' is t."
  (let* ((result nil)
         (visited (make-hash-table))
         (start-domain (? origin :domain)))
    (labels
        ((recursive-walk (x y z location)
           (push (list x y z location) result)
           (sethash (entity-label location) visited t)
           (when (or cross-domains (eq (? location :domain) start-domain))
             (dolist (exit (? location :exits))
               (destructuring-bind (dx dy dz) (direction-offset (exit-dir exit))
                 (when (and (or up-down (= dz 0))
                            (or (/= dx 0) (/= dy 0))
                            (<= (max (abs (+ x dx)) (abs (+ y dy)) (abs (+ z dz))) radius))
                   (let ((dest (and (or (null observer) t)  ; FIXME: check if visible
                                    (find-location (exit-dest exit)))))
                     (when (and dest (null (gethash (entity-label dest) visited)))
                       (recursive-walk (+ x dx) (+ y dy) (+ z dz) dest)))))))))
      (recursive-walk 0 0 0 origin)
      (nreverse result))))

(defun exit-visible-p (exit observer)
  ;; FIXME:
  ;; (or (visiblep portal observer)
  ;;     (let ((dest (find-location (destination portal))))
  ;;       (eq dest (location observer)))))
  t)

;;; Bits that define the state of a location, in addition to the bits defined
;;; for exit directions (see location.lisp).

(defconstant +available-map-bit+ (ash 1 12))
(defconstant +incomplete-map-bit+ (ash 1 13))
(defconstant +complete-map-bit+ (ash 1 14))
(defconstant +vendor-map-bit+ (ash 1 15))
(defconstant +trainer-map-bit+ (ash 1 16))

(defun location-state (location avatar)
  (let ((contents (? location :contents)))
    (apply #'logior
           (case nil ; FIXME: (location-quest-state avatar location)
             (:available +available-map-bit+)
             (:incomplete +incomplete-map-bit+)
             (:complete +complete-map-bit+)
             (t 0))
           (if (some (lambda (x) (typep x 'vendor)) contents) +vendor-map-bit+ 0)
           (if (some (lambda (x) (typep x 'trainer)) contents) +trainer-map-bit+ 0)
           (loop for exit in (? location :exits)
                 if (exit-visible-p exit avatar)
                   collect (direction-map-bit (exit-dir exit))))))
