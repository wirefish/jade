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

(defcommand help (actor "help" topic)
  "The help command can be used to get information on a variety of general topics
or details about a specific command. For information on a specific topic or
command, type `help` followed by the subject of interest; for example, `help
movement`."
  (setf topic (or (first (gethash (first topic) *aliases*)) (first topic)))
  (cond
    ((or (null topic) (string= topic "help"))
     ;; Display the help message defined by the help command itself followed by
     ;; lists of commands and topics.
     (show-help actor (command-help (find-command "help")))
     (show-links actor "Help is available for the following commands:" "help"
                 (remove "help" (get-all-command-verbs) :test #'string=))
     (show-links actor "Help is also available for the following general topics:" "help"
                 (get-help-topics)))
    (t
     (if-let ((command (find-command topic)))
       (show-help actor (command-help command))
       (if-let ((help-text (read-help-file topic)))
         (show-help actor help-text)
         (show actor "There is no help available on that topic."))))))
