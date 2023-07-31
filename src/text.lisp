(in-package :jade)

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

(defun singular-and-plural (s)
  (if-let ((start (position #\[ s)))
    (let* ((sep (position #\| s :start start))
           (end (position #\] s :start start))
           (prefix (subseq s 0 start))
           (suffix (subseq s (+ end 1))))
      (if (null sep)
          (values
           (strcat prefix suffix)
           (strcat prefix (subseq s (+ start 1) end) suffix))
          (values
           (strcat prefix (subseq s (+ start 1) sep) suffix)
           (strcat prefix (subseq s (+ sep 1) end) suffix))))
    (values s (guess-plural-noun s))))

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
             (singular plural (singular-and-plural noun)))
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
               (t (noun-singular noun))))
            ;; In the plural case, prepend the actual number unless quantity is
            ;; t, in which case optionally prepend the definite article.
            ((eq quantity t)
             (if (eq article :definite)
                 (format nil "the ~a" (noun-plural noun))
                 (noun-plural noun)))
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
      (bind ((singular plural (singular-and-plural s)))
        (make-verb :singular singular :plural plural))
      (let* ((sep (position #\space s))
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
