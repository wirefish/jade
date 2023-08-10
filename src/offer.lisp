(in-package :jade)

(defun make-offer (avatar fn &rest args)
  "Sets the pending offer for `avatar'. When the offer is eventually accepted or
rejected, `fn' will be called with `args' and one final argument that is t if
the offer was accepted or nil if it was rejected."
  (with-slots (pending-offer) avatar
    (when pending-offer (funcall pending-offer nil))
    (setf pending-offer (apply #'curry fn args))))

(defun reject-offer (avatar)
  "Rejects the pending offer for `avatar', if any."
  (when-let ((offer (pending-offer avatar)))
    (setf (pending-offer avatar) nil)
    (funcall offer nil)))

(defcommand accept (actor ("accept" "ac"))
  "Accept the most recent offer made to you. The offer could be for any number of
things, such as a quest, a trade, or a party invitation. You can have at most
one active offer at a time; an offer is canceled when you move or when it is
replaced by a newer offer."
  (if-let ((offer (pending-offer actor)))
    (progn
      (setf (pending-offer actor) nil)
      (funcall offer t))
    (show actor "You have not been offered anything.")))

(defcommand reject (actor "reject")
  "Reject the most recent offer made to you."
  (if (pending-offer actor)
    (reject-offer actor)
    (show actor "You have not been offered anything.")))
