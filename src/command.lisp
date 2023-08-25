(in-package :jade)

(defstruct command verbs clauses body)

(defun format-command-alts (alts)
  (if (second alts)
      (format nil "~a (or ~{~a~^, ~})" (first alts) (rest alts))
      (first alts)))

(defun command-syntax (command)
  (format nil "> Syntax:&ensp;~a~{&ensp;&lt;~a&gt;~}~%~%"
          (format-command-alts (command-verbs command))
          (mapcar #'(lambda (clause)
                      (destructuring-bind (name &rest preps) clause
                        (case (first preps)
                          ((nil &word &rest &raw) (format nil "*~(~a~)*" name))
                          (t (format nil "~a *~(~a~)*" (format-command-alts preps) name)))))
                  (command-clauses command))))

(defun command-help (command)
  (strcat (command-syntax command)
          (documentation (command-body command) t)))

(defvar *commands* (make-hash-table :test #'equal))

(defun register-command (verbs clauses body)
  (let ((command (make-command :verbs verbs :clauses clauses :body body)))
    (dolist (verb verbs)
      (setf (gethash verb *commands*) command))))

(defun find-command (verb)
  (gethash verb *commands*))

;;;

(defun parse-grammar (terms)
  "Turns a command grammar into a list of lists, where each sublist contains the
parameter name followed by either &word, &rest, or zero or more prepositions.
Throws an error if the grammar is invalid."
  (labels ((require-symbol (term)
             (if (and term (symbolp term))
                 term
                 (error "symbol expected at ~a" term)))
           (require-preps (term)
             (let ((preps (ensure-list term)))
               (if (every #'stringp preps)
                   preps
                   (error "string or list of strings expected at ~a" term))))
           (parse (terms prev)
             (when-let ((term (first terms)))
               (cond
                 ((eq prev '&rest)
                  (error "grammar cannot define clauses after a &rest clause"))
                 ((or (eq term '&word) (eq term '&rest))
                  (unless (or (eq prev t) (eq prev '&word))
                    (error "~a clause cannot follow a prepositional clause" term))
                  (cons (list (require-symbol (second terms)) term)
                        (parse (cddr terms) term)))
                 ((symbolp term)
                  (unless (or (eq prev t) (eq prev '&word))
                    (error "only the first prepositional clause may have no prepositions"))
                  (cons (list term)
                        (parse (cdr terms) nil)))
                 (t
                  (cons (cons (require-symbol (second terms)) (require-preps term))
                        (parse (cddr terms) nil)))))))
    (parse terms t)))

(defun parse-verbs (verbs)
  (let ((verbs (ensure-list verbs)))
    (if (every #'stringp verbs)
        verbs
        (error "command verbs must be a string or list of strings"))))

(defmacro defcommand (name (actor verbs &rest clauses) &body body)
  (handler-case
      (bind ((verbs (parse-verbs verbs))
             (clauses (parse-grammar clauses))
             (params (cons actor (mapcar #'car clauses)))
             (body nil doc (parse-body body :documentation t)))
        `(register-command
          ',verbs ',clauses
          (lambda ,params
            ,(or doc "No documentation provided.")
            (declare (ignorable ,@params))
            (block ,name
              ,@body))))
    (error (e)
      (format-log :warning "error defining command ~a: ~a" e)
      nil)))

;;;

(defun get-primary-command-verbs ()
  (sort (remove-duplicates (loop for command being the hash-values in *commands*
                                 collect (first (command-verbs command))))
        #'string<))

(defvar *aliases* (make-hash-table :test #'equal))

(defun make-alias (alias command)
  (setf (gethash alias *aliases*) (tokenize-input command)))

;;; Functions that may be helpful when implementing commands.

(defun join-tokens (tokens)
  (format nil "~{~a~^ ~}" tokens))

(defun match-action (actor obj &optional arg)
  (typecase obj
    (string (show actor obj arg))
    (function (if arg (funcall obj arg) (funcall obj)))))

(defun match (actor tokens policy subjects
              &key no-tokens no-subjects no-match multi-match)
  "Match `tokens` against `subjects' and return results subject to `policy'. The
keyword arguments specify actions to take if the match attempt fails.

If `subjects' is null, then the match always fails. If `tokens' is present, then
the `no-match' action applies; otherwise, the `no-subjects' action applies.

If `tokens' is null, then if `no-tokens` is t, all subjects are considered to
match. Otherwise the match fails with `no-match'.

If both `tokens' and `subject' are provided, the match succeeds or fails based
on `policy' and the number of matches. If `policy' is :exactly-one and there are
multiple matches, the match fails with `multi-match'. Otherwise, the match
succeeds. The primary return value is the single match (for :exactly-one) or the
list of matches (for :at-least-one). The secondary value is the match quality,
which can be nil if `tokens' is null.

The failure actions can either be strings or functions. If they are strings,
they are shown to `actor' after being formatted with one argument in the following cases:

- no-match: the argument is a string representing `tokens'.

- multi-match: the argument is a string representing the description of the
  matches.

If the `no-subjects' action applies but is nil, the the `no-match' action is
used instead. This can be useful when `tokens' is known to be present.

If an action is a function, it is called with at most one argument, as described
above."
  (cond
    (subjects
     (bind ((matches quality (if tokens
                                 (find-matches tokens subjects)
                                 (and (eq no-tokens t) subjects))))
       (case (length matches)
         (0
          (if tokens
              (when no-match
                (match-action actor no-match (join-tokens tokens)))
              (when no-tokens
                (match-action actor no-tokens)))
          nil)
         (1
          (values (if (eq policy :exactly-one) (first matches) matches) quality))
         (t
          (if (eq policy :at-least-one)
              (values matches quality)
              (prog1 nil
                (when multi-match
                  (match-action actor multi-match
                                (format-list #'describe-brief matches "or")))))))))
    (t
     (cond
       ((or tokens (null no-subjects))
        (when no-match
          (match-action actor no-match (join-tokens tokens)))
        nil)
       (t
        (match-action actor no-subjects)
        nil)))))

(defun match-quantity (actor tokens subjects &rest match-keyword-args)
  (bind ((tokens quantity (split-quantity tokens))
         (match (apply #'match actor tokens :exactly-one subjects
                       match-keyword-args)))
    (when match
        (values match quantity))))

;;;

(defun find-next-preposition (clauses tokens)
  (position-if #'(lambda (token)
                   (some #'(lambda (clause)
                             (let ((preps (cdr clause)))
                               (when (listp preps)
                                 (find token preps :test #'string-equal))))
                         clauses))
               tokens))

(defun parse-clauses (clauses tokens)
  (loop
    for (clause . later-clauses) on clauses
    collect
    (cond
      ((null tokens) nil)
      ((eq (second clause) '&word) (pop tokens))
      ((eq (second clause) '&rest) (prog1 tokens (setf tokens nil)))
      (t
       (let ((prep (find (car tokens) (rest clause) :test #'string-equal))
             (next-prep (find-next-preposition later-clauses tokens)))
         (unless (eql next-prep 0)
           (prog1
               (subseq tokens (if prep 1 0) next-prep)
             (setf tokens (if next-prep (subseq tokens next-prep) nil)))))))))

(defun process-input (avatar input)
  "Processes input from a player."
  (when-let ((tokens (tokenize-input input)))
    (let ((verb (string-downcase (first-token-as-string tokens))))
      (when-let ((alias (gethash verb *aliases*)))
        (setf tokens alias
              verb (string-downcase (first-token-as-string tokens))))
      (setf tokens (remove-first-token tokens))
      (if-let ((command (find-command verb)))
        (with-slots (clauses body) command
          (if (eq :raw (-> clauses first cdr))
              (funcall body avatar (remaining-input tokens))
              (apply body avatar (parse-clauses clauses (all-tokens-as-strings tokens)))))
        (show-error avatar "Unknown command ~s. Type \"help\" for help." verb)))))
