(in-package :jade)

;;;

(defun parse-prepositions (s)
  (cond
    ((string= s "1") :one-word)
    ((string= s "+") :all-words)
    (t (cl-ppcre:split "\\|" s))))

(defun parse-grammar (s)
  (bind (((actor verbs &rest clauses) (cl-ppcre:split "\\s+" s))
         (actor (intern (string-upcase actor)))
         (verbs (cl-ppcre:split "\\|" verbs))
         (clauses (loop for clause in clauses
                        collect (bind (((preps var) (cl-ppcre:split ":" clause))
                                       (var (intern (string-upcase var)))
                                       (preps (parse-prepositions preps)))
                                  (cons preps var)))))
    (list actor verbs clauses)))

;;;

(defstruct command verbs clauses body)

(defun format-command-alts (alts)
  (if (second alts)
      (format nil "~a (or ~{~a~^, ~})" (first alts) (rest alts))
      (first alts)))

(defun command-syntax (command)
  (format nil "> Syntax:&ensp;~a~{&ensp;&lt;~a&gt;~}~%~%"
          (format-command-alts (command-verbs command))
          (mapcar #'(lambda (clause)
                      (destructuring-bind (verbs . name) clause
                        (case verbs
                          ((nil :word :rest) (format nil "*~(~a~)*" name))
                          (t (format nil "~a *~(~a~)*" (format-command-alts verbs) name)))))
                  (command-clauses command))))

(defun command-help (command)
  (strcat (command-syntax command)
          (documentation (command-handler command) t)))

(defvar *commands* (make-hash-table :test #'equal))

(defun register-command (verbs clauses body)
  (let ((command (make-command :verbs verbs :clauses clauses :body body)))
    (dolist (verb verbs)
      (setf (gethash verb *commands*) command))))

(defmacro defcommand (grammar-spec &body body)
  (bind (((actor verbs clauses) (parse-grammar grammar-spec))
         (params (cons actor (mapcar #'cdr clauses)))
         (clauses (mapcar #'car clauses)))
    `(register-command
      ',verbs ',clauses
      (lambda ,params
        (declare (ignorable ,@params))
        ,@body))))

(defun get-all-command-verbs ()
  (sort (remove-duplicates (loop for command being the hash-values in *commands*
                                 collect (first (command-verbs command))))
        #'string<))

;;;

(defun tokenize-input (input)
  "Returns a list containing the sequence of whitespace-delimited tokens in a
  user input string. If the first character is not alphanumeric, it always forms
  its own token even if not followed by whitespace."
  (let ((input (string-left-trim #(#\Space #\Tab #\Return #\Newline) input)))
    (when (> (length input) 0)
      (if (alphanumericp (char input 0))
          (cl-ppcre:split "\\s+" input)
          (cons (subseq input 0 1) (cl-ppcre:split "\\s+" input :start 1))))))

(defvar *aliases* (make-hash-table :test #'equal))

(defun make-alias (alias command)
  (setf (gethash alias *aliases*) (tokenize-input command)))

(defun strip-preposition (preps tokens)
  (if (find (first tokens) preps :test #'string-equal)
      (rest tokens)
      tokens))

(defun find-next-preposition (clauses tokens)
  (position-if #'(lambda (token)
                   (some #'(lambda (clause)
                             (find token (car clause) :test #'string-equal)) clauses))
               tokens))

(defun parse-command-args (clauses tokens)
  (do (results)
      ((null clauses) (nreverse results))
    (let ((clause (car (pop clauses))))
      ;; clause is (verbs . arg-name)
      (cond
        ;; No more input to assign to this clause.
        ((null tokens)
         (push nil results))
        ;; Consume a single word.
        ((eq clause :word)
         (push (first tokens) results)
         (pop tokens))
        ;; Consume the rest of the input.
        ((eq clause :rest)
         (push tokens results)
         (setf tokens nil))
        ;; Consume until the next preposition, or end of input.
        (t
         (let ((end (find-next-preposition clauses tokens)))
           (if (null end)
               (progn
                 (push (strip-preposition clause tokens) results)
                 (setf tokens nil))
               (progn
                 (push (strip-preposition clause (subseq tokens 0 end)) results)
                 (setf tokens (subseq tokens end))))))))))

(defun find-command (verb)
  (gethash verb *commands*))

(defun run-command (actor command tokens)
  (with-slots (clauses handler) command
    (apply handler actor (parse-command-args clauses tokens))))

(defun process-input (avatar input)
  "Processes input from a player."
  (let ((tokens (tokenize-input input)))
    (when tokens
      (let* ((verb (string-downcase (first tokens)))
             (alias (gethash verb *aliases*)))
        (when alias
          (setf tokens alias)
          (setf verb (first tokens)))
        (let ((command (find-command verb)))
          (if command
              (run-command avatar command (rest tokens))
              (show-error avatar "Unknown command ~s. Type \"help\" for help.~%" verb)))))))
