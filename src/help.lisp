(in-package :jade)

(defun help-directory ()
  (merge-pathnames "data/help/" (gethash :root-directory *config*)))

(defun get-help-topics ()
  (mapcar #'pathname-name
          (directory (merge-pathnames "*.md" (help-directory)))))

(defun read-help-file (topic)
  (let ((path (merge-pathnames (format nil "~a.md" topic) (help-directory))))
    (with-open-file (stream path :if-does-not-exist nil)
      (when stream
        (read-stream-content-into-string stream)))))

(defcommand (help :allow-dead t) (actor "help" &word topic subtopic)
  "The help command can be used to get information on a variety of general topics
or details about a specific command. For information on a specific topic or
command, type `help` followed by the subject of interest; for example, `help
movement`."
  (when-let ((alias (gethash topic *aliases*)))
    (show actor "~s is an alias for ~s." topic (remaining-input alias))
    (setf topic (first-token-as-string alias)))
  (cond
    ((or (null topic) (string= topic "help"))
     ;; Display the help message defined by the help command itself followed by
     ;; lists of commands and topics.
     (show-help actor (command-help (gethash 'help *commands*)))
     (show-links actor "Help is available for the following commands:" "help"
                 (remove "help"
                         (sort (mapcar (lambda (c) (first (command-verbs c)))
                                       (hash-table-values *commands*))
                               #'string<)
                         :test #'string=))
     (show-links actor "Help is also available for the following general topics:" "help"
                 (get-help-topics)))
    (t
     (if-let ((commands (match-commands topic)))
       (cond
         ((= (length commands) 1)
          (show-help actor (command-help (cdar commands))))
         ((> (length commands) 1)
          (show actor "Do you want help with ~a?"
                (format-list (lambda (x) (format nil "`help:~a`" (car x))) commands "or")))
         (t
          (if-let ((help-text (read-help-file topic)))
            (show-help actor help-text)
            (show actor "There is no help available on that topic."))))))))
