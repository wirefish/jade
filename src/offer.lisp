(in-package :jade)

(defgeneric accept-offer (avatar offer))

(defgeneric reject-offer (avatar offer))

(defun extend-offer (avatar offer)
  "Sets the pending offer for `avatar'. Any existing offer will be rejected."
  (with-slots (pending-offer) avatar
    (when pending-offer (reject-offer avatar pending-offer))
    (setf pending-offer offer)))

(defun reject-pending-offer (avatar)
  "Rejects the pending offer for `avatar', if any."
  (with-slots (pending-offer) avatar
    (when pending-offer
      (reject-offer avatar pending-offer)
      (setf pending-offer nil))))

(defcommand accept (actor ("accept" "ac"))
  "Accept the most recent offer made to you. The offer could be for any number of
things, such as a quest, a trade, or a party invitation. You can have at most
one active offer at a time; an offer is canceled when you move or when it is
replaced by a newer offer."
  (with-slots (pending-offer) actor
    (if pending-offer
        (progn
          (accept-offer actor pending-offer)
          (setf pending-offer nil))
        (show actor "You have not been offered anything."))))

(defcommand reject (actor "reject")
  "Reject the most recent offer made to you. An offer is automatically rejected
when you move or when it is replaced by a newer offer."
  (with-slots (pending-offer) actor
    (if pending-offer
        (reject-pending-offer actor)
        (show actor "You have not been offered anything."))))
