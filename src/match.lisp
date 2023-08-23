;;;; Functions that tokenize user input and match it against in-game objects.

(in-package :jade)

;;; Tokenizing

(defparameter *token-pattern* (cl-ppcre:create-scanner "[#\\w'\"-]+|[!?,.]+"))

(defun tokenize-input (input)
  (when-let ((tokens (cl-ppcre:all-matches *token-pattern* input)))
    (cons input tokens)))

(defun first-token-as-string (tokens)
  (when-let ((start (second tokens))
             (end (third tokens)))
    (subseq (first tokens) start end)))

(defun all-tokens-as-strings (tokens)
  (loop for (start end) on (rest tokens) by #'cddr
        collect (subseq (first tokens) start end)))

(defun remove-first-token (tokens)
  (cons (car tokens) (cdddr tokens)))

(defun remaining-input (tokens)
  (when-let ((start (second tokens)))
    (subseq (first tokens) start)))

;;; Matching

(defgeneric match-subject (tokens subject)
  (:documentation "Evaluates a match between `tokens', which is a sequence of
    strings, and `subject', which is an arbitrary object. Returns one of
    `:exact', `:partial', or nil to indicate the quality of the match.")
  (:method (tokens subject)
    nil))

(defun match-substring (token string pos)
  "Matches `token' against the substring of `string' starting at position `pos'
and returns the resulting match quality."
  (when (eql (string-not-greaterp token string :start2 pos) (length token))
    (if (or (= (- (length string) pos) (length token))
            (char= #\Space (char string (+ pos (length token)))))
        :exact
        :partial)))

(defun next-word-position (s pos)
  "Returns the position of the first character of the next word in `s' that
follows the word at `pos', or nil if there are no more words."
  (let ((sep (position #\Space s :start pos)))
    (and sep
         (position-if-not #'(lambda (c) (char= c #\Space)) s :start sep))))

(defmethod match-subject (tokens (subject string))
  "Matches a sequence of strings `tokens' against the string `subject'. Returns
`:exact' if the tokens are exact matches for all the space-delimited words in
`subject', `:partial' if the tokens are prefixes of a subset of the words in
`subject', or `nil' otherwise."
  (do ((pos 0)
       (match nil))
      ((or (null tokens) (null pos))
       (if (null tokens)
           (if (and pos (eq match :exact)) :partial match)))
    (case (match-substring (first tokens) subject pos)
      ((nil)
       (setf match :partial))
      (:partial
       (setf match :partial)
       (pop tokens))
      (:exact
       (when (null match) (setf match :exact))
       (pop tokens)))
    (when pos
      (setf pos (next-word-position subject pos)))))

(defmethod match-subject (tokens (subject noun))
  "Matches `tokens' against the singular and plural forms of the noun `subject'.
Returns one of `:exact', `:partial', or `nil' depending on the quality of the
better match. The second return value is :singular or :plural, denoting the
better match."
  (with-slots (singular plural) subject
    (let ((smatch (match-subject tokens singular)))
      (if (or (null plural) (eq smatch :exact))
          (values smatch :singular)
          (if-let ((pmatch (match-subject tokens plural)))
            (values pmatch (when (eq pmatch :exact) :plural))
            (values smatch nil))))))

(defun best-match-quality (&rest matches)
  "Returns the best match quality from `matches', which is a list whose elements
are `:exact', `:partial', or `nil'."
  (do (best)
      ((or (eq best :exact) (null matches)) best)
    (case (pop matches)
      (:exact (setf best :exact))
      (:partial (when (not best) (setf best :partial))))))

(defun match-subjects (tokens &rest subjects)
  "Returns the best match quality resulting from matching `tokens' against each
element of `subjects'."
  (do (match)
      ((or (eq match :exact) (null subjects)) match)
    (case (match-subject tokens (pop subjects))
      (:exact (setf match :exact))
      (:partial (when (not match) (setf match :partial))))))

(defmethod match-subject (tokens (subject entity))
  (unless (? subject :unmatchable)
    (if-let ((id-token (find-if (curry #'starts-with #\#) tokens)))
      ;; The integer part of `id-token' must be non-nil and exactly match the
      ;; identifier of `subject'. Any other input tokens are ignored.
      (let ((id (parse-integer id-token :start 1 :junk-allowed t)))
        (when (and id (eql id (entity-id subject))) :exact))
      ;; Match `tokens' against matchable properties of `subject'.
      (apply #'match-subjects tokens (? subject :name) (? subject :brief) (? subject :alts)))))

(defmethod match-subject (tokens (subject cons))
  (match-subject tokens (cdr subject)))

(defun find-matches (tokens subjects)
  "Returns a list of subjects that represent the best matches between `tokens'
and elements of `subjects'. If any match is exact, only exact matches are
returned. Otherwise, all partial matches are returned. The secondary value
indicates the best match quality and is one of `:exact', `:partial', or `nil'."
  (let (exact-matches partial-matches)
    (dolist (subject subjects)
      (case (match-subject tokens subject)
        (:exact
         (push subject exact-matches))
        (:partial
         (when (not exact-matches)
           (push subject partial-matches)))))
    (values (or exact-matches partial-matches)
            (cond
              (exact-matches :exact)
              (partial-matches :partial)))))

(defun find-matches-if (pred tokens subjects)
  "Like `find-matches' but considers only those subjects for which `pred' does not
return `nil'."
  (find-matches tokens (remove-if-not pred subjects)))
