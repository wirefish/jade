(in-package :jade)

(defclass command ()
  ((verbs :initform nil :initarg :verbs :reader command-verbs)
   (clauses :initform nil :initarg :clauses :reader command-clauses)
   (body :initform nil :initarg :body :reader command-body)
   (allow-dead :initform nil :initarg :allow-dead :reader command-allow-dead)))

;;;

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
          (if (command-allow-dead command)
              (format nil "(This command can be used when you're dead.)~%~%")
              "")
          (documentation (command-body command) t)))

;;;

;; A hash table mapping from each command's name to the command struct itself.
(defvar *commands* (make-hash-table))

;; A mapping from aliases to the resulting input string.
(defvar *aliases* (make-hash-table :test #'equal))

;; A sorted vector of (verb . command) for all verbs associated with each
;; command.
(defparameter *sorted-commands* nil)

(defun add-command (name verbs clauses args body)
  (let ((command (apply #'make-instance 'command :verbs verbs :clauses clauses :body body args)))
    (setf *sorted-commands* nil)
    (setf (gethash name *commands*) command)))

(defun add-command-alias (alias command)
  (setf (gethash alias *aliases*) (tokenize-input command)))

(defun require-sorted-commands ()
  "If *sorted-commands* is nil, rebuilds it using all currently known commands."
  (unless *sorted-commands*
    (setf *sorted-commands*
          (sort (loop for name being the hash-keys in *commands* using (hash-value command)
                      nconc (loop for verb in (command-verbs command)
                                  collect (cons verb command)))
                #'string< :key #'car))))

(defun match-commands (verb)
  "Given an input token `verb', returns all of the matching commands."
  (require-sorted-commands)
  (when-let ((matches (member-if (lambda (s) (string>= s verb)) *sorted-commands* :key #'car)))
    (remove-duplicates
     (cond
       ((string= (caar matches) verb)
        (list (car matches)))
       (t
        (let ((end (position-if-not (lambda (s) (string-starts-with s verb)) matches :key #'car)))
          (if end
              (subseq matches 0 end)
              matches))))
     :key #'cdr)))

;;;

(defun parse-grammar (terms)
  "Turns a command grammar into a list of lists, where each sublist contains the
parameter name followed by either &word, &rest, or zero or more prepositions.
Clause ordering is constrained: any &word clauses must appear first, then either
a single &rest clause or zero or more prepositional clauses may appear. Throws
an error if the grammar is invalid."
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
                  (unless (or (null prev) (eq prev '&word))
                    (error "~a clause cannot follow a prepositional clause" term))
                  (cons (list (require-symbol (second terms)) term)
                        (parse (cddr terms) term)))
                 ((symbolp term)
                  (unless (or (null prev) (eq prev '&word))
                    (error "only the first prepositional clause may have no prepositions"))
                  (cons (list term)
                        (parse (cdr terms) t)))
                 (t
                  (cons (cons (require-symbol (second terms)) (require-preps term))
                        (parse (cddr terms) t)))))))
    (parse terms nil)))

(defun parse-verbs (verbs)
  (let ((verbs (ensure-list verbs)))
    (if (every #'stringp verbs)
        verbs
        (error "command verbs must be a string or list of strings"))))

(defmacro defcommand (name-and-args (actor verbs &rest clauses) &body body)
  (handler-case
      (bind (((name &rest args) (ensure-list name-and-args))
             (verbs (parse-verbs verbs))
             (clauses (parse-grammar clauses))
             (params (cons actor (mapcar #'car clauses)))
             (body nil doc (parse-body body :documentation t)))
        `(add-command
          ',name ',verbs ',clauses ',args
          (lambda ,params
            ,(or doc "No documentation provided.")
            (declare (ignorable ,@params))
            (block ,name
              ,@body))))
    (error (e)
      (format-log :warning "error defining command ~a: ~a" e)
      nil)))

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

(defun parse-clauses (grammar tokens)
  (let (clauses)
    ;; Handle any &word clauses.
    (loop while (eq (second (first grammar)) '&word) do
      (let ((token (first-token-as-string tokens)))
        (push token clauses)
        (setf tokens (remove-first-token tokens))
        (pop grammar)))
    ;; Handle any &rest clause.
    (when (eq (second (first grammar)) '&rest)
      (push (remaining-input tokens) clauses)
      (setf grammar nil))
    ;; Handle any prepositional clauses.
    (setf tokens (all-tokens-as-strings tokens))
    (loop for (clause . later-clauses) on grammar
          collect
          (let ((prep (find (car tokens) (rest clause) :test #'string-equal))
                (next-prep (find-next-preposition later-clauses tokens)))
            (push (unless (eql next-prep 0)
                    (prog1 (subseq tokens (if prep 1 0) next-prep)
                      (setf tokens (if next-prep (subseq tokens next-prep) nil))))
                  clauses)))
    (nreverse clauses)))

(defun process-input (avatar input)
  "Processes input from a player."
  (when-let ((tokens (tokenize-input input)))
    (let ((verb (string-downcase (first-token-as-string tokens))))
      (when-let ((alias (gethash verb *aliases*)))
        (setf tokens alias
              verb (string-downcase (first-token-as-string tokens))))
      (setf tokens (remove-first-token tokens))
      (let ((commands (match-commands verb)))
        (cond
          ((null commands)
           (show-error avatar "Unknown command ~s. Type \"help\" for help." verb))
          ((> (length commands) 1)
           (show-error avatar "Ambiguous command. Do you mean ~a?"
                       (format-list #'car commands "or")))
          (t
           (let ((command (cdar commands)))
             (if (and (is-dead avatar) (not (command-allow-dead command)))
                 (show avatar "You are too dead to do that right now.")
                 (with-slots (clauses body) command
                   (apply body avatar (parse-clauses clauses tokens)))))))))))
