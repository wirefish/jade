;;;; Functions and commands related to inspecting entities.

(in-package :jade)

;;;

(defgeneric inspect (actor subject tool)
  (:documentation "Called when `actor' inspects `subject' using an optional `tool'."))

(defmethod inspect :around (actor subject tool)
  (let ((observers (list actor subject (entity-container subject) tool)))
    (notify-observers observers :before-inspect actor subject tool)
    (call-next-method)
    (notify-observers observers :after-inspect actor subject tool)))

(defmethod inspect ((actor avatar) (subject location) tool)
  (clear-client-state actor :location-name :location-description)
  (show-location actor subject))

(defmethod inspect ((actor avatar) subject tool)
  ;; TODO: if when-inspect is handled, use that instead.
  (show actor (describe-full subject))
  (when tool
    (show actor "Using ~a does not reveal anything special."
          (describe-brief tool :quantity 1))))

(defcommand look (actor "look" "at" subject ("in" "on") container ("with" "using") tool)
  "Look at something in your environment. By just typing `look`, you will see a
description of your current location and a summary of the entities present
there. To look at something specific, add *subject*. For example, `look orc` or
`look at self`. (The \"at\" is optional.)

Normally the command will look for entities in your location. To look at
something in a specific place, add *container*. For example, `look at dagger on
desk`. Finally, add *tool* to use a specific item in your inventory to aid your
inspection. For example, `look at jewel with magnifying glass`.

To look at items in your inventory or equipment, use the `inventory` command."
  (bind ((location (location actor))
         (tool (when tool
                 (or (match actor tool :exactly-one (? actor :inventory)
                       :no-match "You aren't carrying anything that matches ~s."
                       :multi-match "Do you want to use ~a?")
                   (return-from look))))
        (container (when container
                     (or (match actor container :exactly-one (? location :contents)
                           :no-match "There is no container here that matches \"~a\"."
                           :multi-match "Do you want to look in ~a?")
                         (return-from look)))))
    (cond
      ;; Look at the actor's location.
      ((and (null subject) (null container))
       (inspect actor location tool))
      ;; Look at the actor.
      ((and (equalp subject '("self")) (null container))
       (inspect actor actor tool))
      ;; Look at things in a container.
      (container
       (if (has-attributes container :contents)
           (when-let ((matches
                       (match actor subject :at-least-one (? container :contents)
                         :no-tokens t
                         :no-subjects (format nil "There is nothing in ~a."
                                              (describe-brief container :article :definite))
                         :no-match (format nil "There is nothing in ~a that matches ~~s."
                                           (describe-brief container :article :definite)))))
             (if subject
                 (dolist (item matches)
                   (inspect actor item tool))
                 (show actor "~a contains ~a."
                       (describe-brief container :article :definite :capitalize t)
                       (format-list #'describe-brief matches))))
           (show actor "You cannot look inside ~a."
                 (describe-brief container))))
      ;; Look at specific things nearby.
      (t
       (when-let ((matches (match actor subject :at-least-one (? location :contents)
                             :no-match "There is nothing here that matches ~s.")))
         (dolist (entity matches)
           (inspect actor entity tool)))))))

;;;

(defcommand dump (actor "dump" subject)
  "Prints the raw representation of matching entities."
  (cond
    ((or (null subject) (equalp subject '("self")))
     (show-raw actor (write-to-string (encode-entity actor))))
    (t
     (if-let ((matches (find-matches subject (? (location actor) :contents))))
       (loop for match in matches do
         (show-raw actor (write-to-string (encode-entity match))))
       (show actor "There is nothing here that matches \"~a\"."
             (join-tokens subject))))))
