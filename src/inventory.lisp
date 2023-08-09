;;;; Commands that allow a player to interact with an avatar's inventory in
;;;; various ways.

(in-package :jade)

#|

The following verbs/events interact with inventory:

- unequip: equipment -> inventory
- equip: inventory -> equipment
- take: environment -> inventory
- put: inventory -> environment
- give: inventory -> npc
- receive: npc -> inventory
- discard: inventory -> /dev/null

|#

;;; Display current inventory and equipment.

(defcommand inventory (actor ("inventory" "inv") pattern)
  "Display a list of the items you are carrying or have equipped. If *pattern* is
present, display a more detailed description of matching items."
  (with-attributes (inventory equipment) actor
    (if pattern
        (if-let ((matches (find-matches pattern (hash-table-alist equipment) inventory)))
          (dolist (item matches)
            (show actor "~a (equipped): ~a"
                  (describe-brief (cdr item) :article nil :capitalize t)
                  (describe-full (cdr item)))
            (show actor "~a: ~a"
                  (describe-brief item :article nil :capitalize t)
                  (describe-full item)))
          (show actor "You are not carrying anything that matches \"~a\"."
                (join-tokens pattern)))
        (progn
          (if (> (hash-table-count equipment) 0)
              (show actor "You have the following items equipped: ~a."
                    (format-list #'describe-brief (hash-table-values equipment)))
              (show actor "You do not have any items equipped."))
          (if inventory
              (show actor "You are carrying ~a." (format-list #'describe-brief inventory))
              (show actor "You are not carrying anything."))))))

;;; Unequip an item: move an item from an equipment slot to inventory.

(defgeneric unequip (actor item slot)
  (:documentation "Called when `actor' attempts to unequip `item' equipped in `slot'."))

(defmethod unequip :around (actor item slot)
  (let ((observers (list actor item)))
    (when (observers-allow-p observers :allow-unequip actor item)
      (notify-observers observers :before-unequip actor item)
      (call-next-method)
      (notify-observers observers :after-unequip actor item)
      t)))

(defmethod unequip ((actor avatar) item slot)
  (remhash slot (? actor :equipment))
  (push item (? actor :inventory))
  (update-equipment actor (list slot))
  (show actor "You place ~a into your backpack." (describe-brief item)))

(defcommand unequip (actor ("unequip" "un") item "from" slot)
  "Unequip an item and return it to your inventory. If *slot* is specified, the
item is removed from a specific equipment slot."
  (declare (ignore slot)) ; FIXME: if slot is present, consider only matching slots.
  (with-attributes (equipment) actor
    (if item
        (bind ((matches quality (find-matches item (hash-table-alist equipment))))
          (cond
            ((null matches)
             (show actor "You don't have anything equipped that matches \"~a\"."
                   (join-tokens item)))
            ((or (eq quality :exact) (= (length matches) 1))
             (loop for (slot . item) in matches do (unequip actor item slot)))
            (t
             (show actor "Do you want to unequip ~a?"
                   (format-list (lambda (m) (describe-brief (cdr m))) matches)))))
        (show actor "Which item do you want to unequip?"))
    (show actor "You do not have any items equipped.")))

;;; Equip an item: move an item from inventory to an equipment slot. If an item
;;; is already in that slot, it is unequipped first. If no item is specified,
;;; show all equipped items.

(defun show-equipment (actor)
  "Displays a list of the items equipped by `actor'."
  (with-attributes (equipment) actor
    ;; List weapons.
    (let (weapons)
      (when-let ((off-hand (gethash :off-hand equipment)))
        (push (format nil "~a in your off-hand" (describe-brief off-hand)) weapons))
      (when-let ((main-hand (gethash :main-hand equipment)))
        (push (format nil (if (eq (? main-hand :equippable-slot) :both-hands)
                              "~a in both hands"
                              "~a in your main hand")
                      (describe-brief main-hand)) weapons))
      (if weapons
          (show actor "You are wielding ~a." (format-list #'identity weapons))
          (show actor "You are not wielding any weapons.")))
    ;; List tools.
    (if-let ((tool (gethash :tool equipment)))
      (show actor "You have ~a at the ready." (describe-brief tool))
      (show actor "You have no tool equipped."))
    ;; List worn equipment.
    (let ((items (delete nil (loop for slot in (member :head *equipment-slots*)
                                   collect (gethash slot equipment)))))
      (if items
          (show actor "You are wearing ~a." (format-list #'describe-brief items))
          (show actor "You are not wearing anything. Brrr!")))))

(defmethod equip :around (actor item slot)
  ;; FIXME: checks
  (let ((observers (list actor item))
        (prev-item (gethash slot (? actor :equipment))))
    (when (and (or (null prev-item) (unequip actor prev-item slot))
               (observers-allow-p observers :allow-equip actor item))
      (notify-observers observers :before-equip actor item)
      (call-next-method)
      (notify-observers observers :after-equip actor item)
      t)))

(defmethod equip ((actor avatar) item slot)
  (remove item (? actor :inventory))
  (sethash slot (? actor :equipment) item)
  (update-equipment actor (list slot))
  (show actor "You equip ~a." (describe-brief item)))

(defcommand equip (actor ("equip" "eq") item "on" slot)
  "When no item is specified, view a list of the items that you currently have equipped.

Otherwise, equip the specified item, moving it from your inventory to the
appropriate equipment slot. Any item that is already equipped in that slot will
be returned to your inventory.

If an item can be equipped in multiple slots, such as a ring which can be worn
on either hand, you can use \"on *slot*\" to specify the slot to use. For
example, `equip gold ring on left finger`."
  ;; FIXME: slot
  (if item
      (bind ((matches quality (find-matches-if (lambda (x) (? x :equippable-slot)) item
                                               (? actor :inventory))))
        (cond
          ((null matches)
           (show actor "You are not carrying anything equippable that matches \"~a\"."
                 (join-tokens item)))
          ((or (eq quality :exact) (= (length matches) 1))
           (equip actor (first matches) slot))
          (t
           (show actor "Do you want to equip ~a?"
                 (format-list #'describe-brief matches)))))
      (show-equipment actor)))

;;; Take an item from a container in the environment and place it into
;;; inventory.

(defgeneric take (actor quantity item container slot)
  (:documentation "Called when `actor' attempts to remove `quantity' of `item'
from `container'."))

(defmethod take :around (actor quantity item container slot)
  (cond
    ((not (entity-isa item 'item))
     (show actor "You cannot take ~a." (describe-brief item :article :definite))
     nil)
    ((> (or (? item :size) +medium+) (or (? actor :size) +medium+))
     (show actor "~a is too large for you to carry."
           (describe-brief item :capitalize t))
     nil)
    (t
     (let ((observers (list actor item container)))
       (when (observers-allow-p observers :allow-take actor item container)
         (notify-observers observers :before-take actor item container)
         (notify-observers observers :after-take actor (call-next-method) container)
         t)))))

(defmethod take (actor quantity item container slot)
  (remove-item container slot item quantity))

(defmethod take ((actor avatar) quantity item container slot)
  (let ((removed (call-next-method)))
    (if (eq container (location actor))
        (show actor "You take ~a." (describe-brief removed))
        (show actor "You take ~a from ~a."
              (describe-brief removed)
              (describe-brief container :article :definite)))
    (insert-item actor :inventory removed)
    (check-encumbrance actor)
    removed))

(defun find-containers (tokens candidates)
  (find-matches-if (lambda (x) (has-attributes x :contents)) tokens candidates))

(defcommand take (actor ("take" "get") thing ("from" "in" "on" "off") place)
  "Take an item from your environment and place it into your inventory. If *place*
is provided, it describes an object in your location that contains the items you
want to take. Otherwise, you take items from your location."
  (if thing
      (let ((containers (if place
                            (find-containers place (? (location actor) :contents))
                            (list (location actor)))))
        (case (length containers)
          (0 (show actor "You don't see a container that matches \"~a\"." (join-tokens place)))
          (1 (bind ((container (first containers))
                    (tokens quantity (split-quantity thing))
                    (targets quality (find-matches tokens (? container :contents))))
               (cond
                 ((null targets)
                  (if (eq container (location actor))
                      (show actor "You don't see anything that matches \"~a\"."
                            (join-tokens thing))
                      (show actor "You don't see anything that matches \"~a\" ~a the ~a."
                            (join-tokens thing)
                            "in" ; FIXME: (contents-location container)
                            (describe-brief container :article nil))))
                 ((or (eq quality :exact) (= (length targets) 1))
                  (dolist (target targets)
                    (take actor (or quantity t) target container :contents)))
                 (t
                  (show actor "Do you want to take ~a?"
                        (format-list #'describe-brief targets "or"))))))
          (t (show actor "Do you want to take something from ~a?"
                   (format-list #'describe-brief containers "or")))))
      (show actor "What do you want to take?")))

;;; Place items from inventory into a container in the environment.

(defgeneric put (actor quantity item container slot)
  (:documentation "called when `actor' attempts to place `quantity' of `item'
into `container'."))

(defmethod put :around (actor quantity item container slot)
  (cond
    ((? item :bound)
     (show actor "You cannot drop ~a because it is bound to you."
           (describe-brief item :article :definite))
     nil)
    ((? item :quest)
     (show actor "You cannot drop ~a because it is associated with a quest."
           (describe-brief item :article :definite))
     nil)
    (t
     (let ((observers (list actor item container)))
       (when (observers-allow-p observers :allow-put actor item container)
         (notify-observers observers :before-put actor item container)
         (notify-observers observers :after-put actor (call-next-method) container)
         t)))))

(defmethod put ((actor avatar) quantity item container slot)
  (let ((removed (remove-item actor :inventory item quantity)))
    (if (eq container (location actor))
        (show actor "You drop ~a." (describe-brief removed))
        (show actor "You put ~a ~a ~a."
              (describe-brief removed)
              "in" ; FIXME: (contents-location container)
              (describe-brief container :article :definite)))
    (insert-item container slot removed)
    removed))

(defcommand put (actor ("put" "drop" "place") thing ("in" "on" "into" "onto") place)
  "Remove an item from your inventory and place it into your environment. If
*place* is provided, it describes a container in your location into which you
wish to place the item. Otherwise, you drop the item on the floor at your
location."
  (if thing
      (bind ((tokens quantity (split-quantity thing))
             (items quality (find-matches tokens (? actor :inventory))))
        (cond
          ((null items)
           (show actor "You aren't carrying anything that matches \"~a\"." (join-tokens thing)))
          ((and (eq quality :partial) (> (length items) 1))
           (show actor "Do you want to drop ~a?" (format-list #'describe-brief items "or")))
          (t
           (let ((containers (if place
                                 (find-containers place (? (location actor) :contents))
                                 (list (location actor)))))
             (case (length containers)
               (0
                (show actor "You see nothing matching \"~a\" that can contain items."
                      (join-tokens place)))
               (1
                (dolist (item items)
                  (put actor (or quantity t) item (first containers) :contents)))
               (t
                (show actor "Do you want to place items into ~a?"
                      (format-list #'describe-brief containers "or"))))))))
      (show actor "What do you want to drop?")))

;;; Give an item to an NPC or some other sink.

;; TODO:

;;; Receive a newly-created item from an NPC or some other source.

(defgeneric receive (actor item source)
  (:documentation "Called when `actor' receives `item' from `source'."))

(defmethod receive :around (actor item source)
  (let ((observers (list actor item source)))
    (when (observers-allow-p observers :allow-receive actor item source)
      (notify-observers observers :before-receive actor item source)
      (call-next-method)
      (notify-observers observers :after-receive actor item source)
      t)))

(defmethod receive ((actor avatar) item source)
  ;; FIXME: add a message if resulting encumbrance is over 100%.
  (insert-item actor :inventory item)
  (if source
      (show actor "~a gives you ~a."
            (describe-brief source :article :definite :capitalize t)
            (describe-brief item))
      (show actor "You receive ~a." (describe-brief item)))
  (check-encumbrance actor))


;;; Discard an item.

;; TODO:
