(in-package :jade)

(defun make-message (actor verb-phrase &rest args)
  "Returns a closure that when called with an entity, returns a string that
appropriately describes to that entity an action taken by `actor', or nil if the
entity cannot see `actor'.

`verb-phrase' is a sentence fragment that is initially formatted using `args'
and then parsed using parse-verb. The plural form is used when displaying the
message to `actor' and the singular form is used for any other observer. This
yields results like:

  'You eat an apple.' (when seen by the actor)
  'Bob eats an apple.' (when seen by others)

The resulting closure takes the observer as its first argument. Any subsequent
arguments are used to format the resulting message. For a format directive to
survive until this point it needs to be escaped in the original `verb-phrase',
e.g. ~~a."
  (let* ((verb (parse-verb (apply #'format nil verb-phrase args)))
         (self-message (format nil "You ~a" (verb-plural verb)))
         (other-message (format nil "~a ~a"
                               (describe-brief actor :capitalize t)
                               (verb-singular verb))))
    (lambda (observer &rest args)
      (when (can-see observer actor)
        (let ((msg (if (eq observer actor) self-message other-message)))
          (if args
              (apply #'format nil msg args)
              msg))))))

;;;

(defgeneric show-message (observers arg &rest args))

(defmethod show-message (observers (s string) &rest args)
  (let ((msg (if args (apply #'format nil s args) s)))
    (dolist (observer observers)
        (show observer msg))))

(defmethod show-message (observers (fn function) &rest args)
  (dolist (observer observers)
    (when-let ((msg (apply fn observer args)))
      (show observer msg))))

(defmethod show-message (observers (actor entity) &rest args)
  (show-message observers (apply #'make-message actor args)))
