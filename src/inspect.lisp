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
  (show-location actor subject))

(defmethod inspect ((actor avatar) subject tool)
  ;; TODO: use tool
  (show actor (describe-full subject)))

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
  (let ((tool (when tool
                (or (match-one actor tool (? actor :inventory)
                               "You aren't carrying anything that matches \"~a\"."
                               "Do you want to use ~a?")
                    (return-from look))))
        (container (when container
                     (or (match-one actor container (? (location actor) :contents)
                                    "There is no container here that matches \"~a\"."
                                    "Do you want to look in ~a?")
                         (return-from look)))))
    (cond
      ;; Look at the actor's location.
      ((and (null subject) (null container))
       (inspect actor (location actor) tool))
      ;; Look at the actor.
      ((and (equalp subject '("self")) (null container))
       (inspect actor actor tool))
      ;; Look at specific things.
      (subject
       (if-let ((matches (find-matches subject
                                       (? (or container (location actor)) :contents)
                                       (unless container (? (location actor) :exits)))))
         (loop for match in matches do
           (show actor (describe-full match)))
         (if container
             (show actor "There is nothing in the ~a that matches \"~a\"."
                   (describe-brief container :article :nil)
                   (join-tokens subject))
             (show actor "There is nothing here that matches \"~a\"."
                   (join-tokens subject)))))
      ;; Look at everything in a container.
      (t
       (if-let ((matches (? container :contents)))
         (show actor "The ~a contains ~a."
               (describe-brief  container :article nil)
               (format-list #'describe-brief matches)))))))

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
