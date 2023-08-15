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
                        (case preps
                          ((nil :word :rest :raw) (format nil "*~(~a~)*" name))
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

(defmacro defcommand (name (actor verbs &rest clauses) &body body)
  (bind ((verbs (if (listp verbs) verbs (list verbs)))
         (clauses (when clauses
                    (loop for (preps param) on (if (and (symbolp (car clauses))
                                                        (not (keywordp (car clauses))))
                                                   (cons nil clauses)
                                                   clauses)
                          by #'cddr
                          collect (cons param (etypecase preps
                                                (keyword preps)
                                                (list preps)
                                                (string (list preps)))))))
         (params (cons actor (mapcar #'car clauses)))
         (body nil doc (parse-body body :documentation t)))
    `(register-command
      ',verbs ',clauses
      (lambda ,params
        ,(or doc "No documentation provided.")
        (declare (ignorable ,@params))
        (block ,name
          ,@body)))))

(defun get-primary-command-verbs ()
  (sort (remove-duplicates (loop for command being the hash-values in *commands*
                                 collect (first (command-verbs command))))
        #'string<))

(defvar *aliases* (make-hash-table :test #'equal))

(defun make-alias (alias command)
  (setf (gethash alias *aliases*) (tokenize-input command)))

;;;

(defun join-tokens (tokens)
  (format nil "~{~a~^ ~}" tokens))

(defun match-one (actor tokens candidates none-message many-message)
  (let ((matches (find-matches tokens candidates)))
    (case (length matches)
      (0
       (show actor none-message (join-tokens tokens))
       nil)
      (1 (first matches))
      (t
       (show actor many-message (format-list #'describe-brief matches "or"))
       nil))))

(defun match-exactly-one (actor tokens candidates no-tokens-message none-message many-message)
  (if tokens
      (match-one actor tokens candidates none-message many-message)
      (progn
        (show actor no-tokens-message)
        nil)))

(defun match-some (actor tokens candidates none-message)
  (let ((matches (find-matches tokens candidates)))
    (or matches
        (progn
          (show actor none-message (join-tokens tokens))
          nil))))

;;;

(defun split-verb (input)
  (cl-ppcre:split "\\s+" input :limit 2))

(defun tokenize-input (input)
  "Returns a list containing the sequence of whitespace-delimited tokens in a user
input string. Any sequence of one or more punctuation characters is considered
its own token, even if not delimited by whitespace."
  (cl-ppcre:all-matches-as-strings "[#\\w'\"-]+|[!?,.]+" input))

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
    for (clause . later-clauses) on clauses for i from 0
    collect
    (let ((preps (cdr clause)))
      (cond
        ((null tokens) nil)
        ((eq preps :word) (pop tokens))
        ((eq preps :rest) (prog1 tokens (setf tokens nil)))
        (t
         (let ((has-prep (find (car tokens) preps :test #'string-equal)))
           (when (or has-prep (= i 0))
             (let ((next-prep (find-next-preposition later-clauses tokens)))
               (unless (eql next-prep 0)
                 (prog1
                     (subseq tokens (if has-prep 1 0) next-prep)
                   (setf tokens (if next-prep (subseq tokens next-prep) nil))))))))))))

(defun run-command (actor command tokens)
  (with-slots (clauses body) command
    (apply body actor (parse-clauses clauses tokens))))

(defun process-input (avatar input)
  "Processes input from a player."
  (when-let ((input (string-trim '(#\Space #\Tab) input)))
    (bind (((verb &optional rest) (split-verb input))
           (verb (string-downcase verb))
           (tokens (cons verb (tokenize-input rest)))
           (alias (gethash verb *aliases*)))
      (when alias
        (setf tokens alias
              verb (first tokens)))
      (if-let ((command (find-command verb)))
        (with-slots (clauses body) command
          (if (eq :raw (-> clauses first cdr))
              (funcall body avatar rest)
              (apply body avatar (parse-clauses clauses (rest tokens)))))
        (show-error avatar "Unknown command ~s. Type \"help\" for help." verb)))))
