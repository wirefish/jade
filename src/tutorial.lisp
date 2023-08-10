(in-package :jade)

(defun maybe-show-tutorial (actor key message)
  (with-slots (tutorials-on tutorials-seen dirty-tutorials) actor
    (when (and tutorials-on (not (gethash key tutorials-seen)))
      (show-tutorial actor message)
      (setf (gethash key tutorials-seen) t)
      (push key dirty-tutorials))))

(defcommand tutorial (actor "tutorial" subcommand)
  "Use this command to control which tutorial text you see. Some rooms have
associated tutorial text that you see the first time you enter. This text is
used to introduce new players to game concepts and commands.

The command has the following subcommands:

- `tutorial on` enables tutorial text. This is the default.

- `tutorial off` disables tutorial text.

- `tutorial reset` clears the set of tutorials you've already seen, so you
  will see them again.

With no subcommand, `tutorial` displays the tutorial for the current location,
if it has one."
  (cond
    ((null subcommand)
     (if-let ((message (? (location actor) :tutorial)))
       (show-tutorial actor message)
       (show actor "There is no tutorial for this location.")))
    ((string-equal (first subcommand) "on")
     (setf (tutorials-on actor) t)
     (show actor "Tutorials are now enabled."))
    ((string-equal (first subcommand) "off")
     (setf (tutorials-on actor) nil)
     (show actor "Tutorials are now disabled."))
    ((string-equal (first subcommand) "reset")
     (clrhash (tutorials-seen actor))
     (setf (dirty-tutorials actor) nil)
     (reset-tutorials (avatar-id actor))
     (show actor "Tutorials have been reset."))
    (t
     (show actor "Unknown subcommand ~s." (first subcommand)))))
