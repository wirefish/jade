(in-package :jade)

;;; General facility for explicitly specifying singular and plural forms of a
;;; noun or verb.

(defparameter *specifier-pattern* (cl-ppcre:create-scanner "\\[(?:([^|]*)\\|)?([^\\]]*)?\\]"))

(defun explicit-singular-and-plural (s)
  "Given string `s' that contains one or more explicit singular/plural
specifiers, returns two string representing the singular and plural forms."
  (labels ((matched-substring (s start end)
             (when start (subseq s start end))))
    (let ((pos 0) singular-parts plural-parts)
      (loop while (< pos (length s)) do
        (multiple-value-bind (spec-start spec-end reg-starts reg-ends)
            (cl-ppcre:scan *specifier-pattern* s :start pos)
          (unless spec-start
            (loop-finish))
          (let ((literal (subseq s pos spec-start)))
            (push literal singular-parts)
            (push literal plural-parts)
            (when-let ((singular (matched-substring s (elt reg-starts 0) (elt reg-ends 0))))
              (push singular singular-parts))
            (when-let ((plural (matched-substring s (elt reg-starts 1) (elt reg-ends 1))))
              (push plural plural-parts)))
          (setf pos spec-end)))
      (when (< pos (length s))
        (let ((literal (subseq s pos)))
          (push literal singular-parts)
          (push literal plural-parts)))
      (values (apply #'strcat (nreverse singular-parts))
              (apply #'strcat (nreverse plural-parts))))))

;;; Noun phrases.

(defun article-p (s)
  "Returns non-nil if `token' is an article."
  (position s '("a" "an" "the") :test #'string-equal))

(defun split-article (s)
  "Given a string `s' that represents a noun phrase consisting of space-separated
  words, returns two values that contain the article (or nil if no article is
  present) and the remainder of the string."
  (let* ((sep (position #\space s))
         (prefix (subseq s 0 sep)))
    (if (article-p prefix)
        (values prefix (subseq s (+ sep 1)))
        (values nil s))))

(defun guess-article (s)
  "Given a string `s' representing a noun phrase with no article, returns a guess
at the appropriate indefinite article."
  (if (find (char s 0) "aeiou") "an" "a"))

(defun guess-plural-noun (s)
  "Given a string `s' that represents a noun phrase, returns a guess at its plural
  form."
  (cond
    ((string-ends-with s "y")
     (if (find (char s (- (length s) 2)) "aeiou")
         (strcat s "s")
         (strcat (subseq s 0 (- (length s) 1)) "ies")))
    ((some #'(lambda (x) (string-ends-with s x)) '("s" "x" "z" "o" "ch" "sh"))
     (strcat s "es"))
    (t
     (strcat s "s"))))

(defstruct noun article singular plural)

(defun parse-noun (s)
  "Parses string `s' to create a noun phrase object. If the string begins with a
capital letter it is interpreted as a proper noun. Otherwise, the string may
begin with an optional article; if none is present, the function guesses based
on the first character of the string. It may also contain a plural specifier
delimited by square brackets. The specifier provides explicit text to include in
the singular and/or plural forms. If no specified is present, the text is
assumed to represent the singular and the function guesses the plural form. Some
examples showing the resulting article, singular, and plural:

  'orange car' -> 'an', 'orange car', 'orange cars'
  'a box[es] of rocks' -> 'a', 'box of rocks', 'boxes of rocks'
  'sarcophag[us|i] -> 'a', 'sarcophagus', 'sarcophagi'
  'Bob' -> nil, 'Bob', nil"
  (if (upper-case-p (char s 0))
      (make-noun :singular s)
      (bind ((article noun (split-article s))
             (singular plural (if (position #\[ s)
                                  (explicit-singular-and-plural noun)
                                  (values noun (guess-plural-noun noun)))))
        (make-noun :article (or article (guess-article noun))
                   :singular singular
                   :plural plural))))

(defun format-noun (noun &key (quantity 1) (article :indefinite) (capitalize nil))
  (let ((s
          (cond
            ;; If it has no article, it's a proper noun and always appears as-is.
            ((not (noun-article noun))
             (noun-singular noun))
            ;; In the singular case, prepend the appropriate article.
            ((eql quantity 1)
             (case article
               (:definite (format nil "the ~a" (noun-singular noun)))
               (:indefinite (format nil "~a ~a" (noun-article noun) (noun-singular noun)))
               (t (copy-seq (noun-singular noun)))))
            ;; In the plural case, prepend the actual number unless quantity is
            ;; t, in which case optionally prepend the definite article.
            ((eq quantity t)
             (if (eq article :definite)
                 (format nil "the ~a" (noun-plural noun))
                 (copy-seq (noun-plural noun))))
            (t
             (format nil "~a ~a" quantity (noun-plural noun))))))
    (when capitalize
      (setf (elt s 0) (char-upcase (elt s 0))))
    s))

;;; Enable #n"..." syntax for creating a noun phrase.

(defun |#n-reader| (stream char arg)
  (declare (ignore char arg))
  (parse-noun (read stream t nil t)))

(set-dispatch-macro-character #\# #\n #'|#n-reader|)

;;; Verb phrases.

(defun guess-plural-verb (s)
  (cond
    ((string= s "is") "are")
    ((string-ends-with s "ies")
     (strcat (subseq s 0 (- (length s) 3)) "y"))
    ((string-ends-with s "es")
     (let ((stem (subseq s 0 (- (length s) 2))))
       (if (string= (guess-plural-noun stem) s)
           stem
           (subseq s 0 (- (length s) 1)))))
    ((string-ends-with s "s")
     (subseq s 0 (- (length s) 1)))
    (t s)))

(defstruct verb singular plural)

(defun parse-verb (s)
  (if (position #\[ s)
      (bind ((singular plural (explicit-singular-and-plural s)))
        (make-verb :singular plural :plural singular))
      (let* ((sep (position-if-not #'alpha-char-p s))
             (prefix (subseq s 0 sep)))
        (make-verb :singular s
                   :plural (strcat (guess-plural-verb prefix) (if sep (subseq s sep) ""))))))

(defun format-verb (verb &key (quantity 1) stream)
  (format stream "~a" (if (= quantity 1) (verb-singular verb) (verb-plural verb))))

;;; Enable #v"..." syntax for creating a verb phrase.

(defun |#v-reader| (stream char arg)
  (declare (ignore char arg))
  (parse-verb (read stream t nil t)))

(set-dispatch-macro-character #\# #\v #'|#v-reader|)

;;; Other text utilities.

(defun format-list (fn items &optional (conjunction "and"))
  "Returns a string that formats the result of applying `fn' to each value in
`items' as a comma-separated list, using `conjunction' before the last item with
an Oxford comma as appropriate."
  (if-let ((items (if fn (mapcar fn items) items)))
    (if (null conjunction)
        (format nil "~{~a~^, ~}" items)
        (case (length items)
          (1 (car items))
          (2 (format nil "~a ~a ~a" (car items) conjunction (cadr items)))
          (t (format nil "~{~a~^, ~}, ~a ~a"
                     (butlast items) conjunction (car (last items))))))
    ""))

(defun parse-quantity (token)
  "Interprets `token' as a possible quantity. Returns either an integer, `t' if
the token indicates 'all', or `nil' if the token does not describe a quantity."
  (multiple-value-bind (num length) (parse-integer token :junk-allowed t)
    (cond
      ((= length (length token)) num)
      ((article-p token) 1)
      ((string-equal token "any") 1)
      ((or (string-equal token "all") (string-equal token "every")) t))))

(defun split-quantity (tokens)
  "Returns two values. If the first token indicates a quantity, the primary
value is the rest of the tokens, and the secondary is the indicated quantity.
Otherwise, the primary value is `tokens' and the secondary value is `nil'."
  (unless (null tokens)
    (if-let ((quantity (parse-quantity (car tokens))))
      (values (cdr tokens) quantity)
      tokens)))
