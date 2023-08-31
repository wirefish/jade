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
                   (let ((dest (and (or (null observer) (can-see observer exit))
                                    (find-location (exit-dest exit)))))
                     (when (and dest (null (gethash (entity-label dest) visited)))
                       (recursive-walk (+ x dx) (+ y dy) (+ z dz) dest)))))))))
      (recursive-walk 0 0 0 origin)
      (nreverse result))))

;;; Bits that define the state of a location, in addition to the bits defined
;;; for exit directions (see location.lisp).

(defconstant +available-map-bit+ (ash 1 12))
(defconstant +incomplete-map-bit+ (ash 1 13))
(defconstant +complete-map-bit+ (ash 1 14))
(defconstant +vendor-map-bit+ (ash 1 15))
(defconstant +trainer-map-bit+ (ash 1 16))

(defparameter *map-quest-events* '(:when-talk :after-give))

(defun quest-advance-event (entity quest-label phase-label)
  "Returns the event that can trigger `entity' to advance the quest named by
`quest-label' for an avatar whose current phase for that quest is named by
`phase-label'."
  (some (lambda (x)
          (when (and (eq (second x) quest-label) (eq (third x) phase-label))
            (first x)))
        (? entity :advances-quests)))

(defun location-quest-state-bit (location avatar)
  (when-let ((candidates (loop for entity in (? location :contents)
                               when (entity-behavior entity)
                                 collect entity)))
    ;; Check if any candidate can advance a quest from the actor's current
    ;; active phase in that quest.
    (loop for (label phase-index . state) in (active-quests avatar) do
      (when-let ((quest (symbol-value-as 'quest label nil)))
        (let ((phase-label (quest-phase-label (quest-get-phase quest phase-index))))
          (when (some (lambda (x)
                        (when-let ((event (quest-advance-event x label phase-label)))
                          (position event *map-quest-events*)))
                      candidates)
            (return-from location-quest-state-bit
              (if (= phase-index (1- (length (quest-phases quest))))
                  +complete-map-bit+
                  +incomplete-map-bit+))))))
    ;; Check if any candidate offers a quest that the actor can accept.
    (loop for entity in candidates do
      (when-let ((quest-labels (? entity :offers-quests)))
        (loop for label in quest-labels do
          (when-let ((quest (symbol-value-as 'quest label nil)))
            (when (can-accept-quest avatar quest)
              (return-from location-quest-state-bit +available-map-bit+)))))))
  0)

(defun location-state (location avatar)
  (let ((contents (? location :contents))
        (avatar-location (entity-label (location avatar))))
    (apply #'logior
           (location-quest-state-bit location avatar)
           (if (some (lambda (x) (typep x 'vendor)) contents) +vendor-map-bit+ 0)
           (if (some (lambda (x) (typep x 'trainer)) contents) +trainer-map-bit+ 0)
           (loop for exit in (? location :exits)
                 when (or (can-see avatar exit)
                          (eq (exit-dest exit) avatar-location))
                   collect (direction-map-bit (exit-dir exit))))))
