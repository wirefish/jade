(in-package :jade)

(defparameter *db* nil)

(defun open-database (root-directory)
  (setf *db* (sqlite:connect (merge-pathnames "jade.db" root-directory)))
  (sqlite:execute-non-query *db* "pragma foreign_keys = on")
  *db*)

(defun close-database ()
  (sqlite:disconnect *db*)
  (setf *db* nil))

(defun hash-password (password)
  "Returns a derived hash of the `password` using random salt."
  (let ((salt (ironclad:make-random-salt))
        (password-bytes (babel:string-to-octets password :encoding :utf-8)))
    (ironclad:pbkdf2-hash-password-to-combined-string
     password-bytes :salt salt :digest :sha1 :iterations 10000)))

(defun validate-username (username)
  "Checks that a proposed username meets some arbitrary requirements. Returns nil
on success or a string describing the reason for failure."
  (cond
    ((< (length username) 3)
     "Username must be at least 3 characters long.")
    ((> (length username) 40)
     "Username may be no more than 40 characters long.")
    ((not (cl-ppcre:scan "^[\\w.,@+-]+$" username))
     "Invalid character in username.")))

(defun validate-password (password)
  "Checks that a proposed password meets some arbitrary requirements. Returns nil
on success or a string describing the reason for failure."
  (cond
    ((< (length password) 8)
     "Password must be at least 8 characters long.")
    ((> (length password) 40)
     "Password may be no more than 40 characters long.")
    ((find-if-not #'graphic-char-p password)
     "Invalid character in password.")))

(defun create-account (username password avatar location)
  "Tries to create an account. Returns the account ID on success or nil on
failure, as when the username is already in use."
  (let ((hashed-password (hash-password password))
        (encoded-avatar (to-string (encode-entity avatar))))
    (handler-case
        (sqlite:with-transaction *db*
          (sqlite:execute-non-query
           *db*
           "insert into accounts (username, password) values (?, ?)"
           username hashed-password)
          (let ((account-id (sqlite:last-insert-rowid *db*)))
            (sqlite:execute-non-query
             *db*
             "insert into avatars (account_id, location, avatar) values (?, ?, ?)"
             account-id (to-string location) encoded-avatar)
            account-id))
      (sqlite:sqlite-error (err)
        (format-log :warning "cannot create account: ~s" err)
        nil))))

(defun find-account-id (username)
  "Returns the account ID associated with `username', or nil if no such account
exists."
  (sqlite:execute-single
   *db*
   "select account_id from accounts where username = ?" username))

(defun delete-account (account-id)
  "Deletes the account and all avatars associated with `account-id'. Returns t on
success or nil if an error occurs."
  (handler-case
      (sqlite:with-transaction *db*
        (sqlite:execute-non-query
         *db*
         "delete from accounts where account_id = ?" account-id)
        t)
    (sqlite:sqlite-error (err)
      (format-log :warning "cannot delete account: ~s" err)
      nil)))

(defun authenticate (username password)
  "Returns the account ID if `password' matches the hashed password stored for the
account associated with `username', or nil otherwise."
  (multiple-value-bind (account-id hashed-password)
      (sqlite:execute-one-row-m-v
       *db*
       "select account_id, password from accounts where username = ?"
       username)
    (when (and hashed-password
               (ironclad:pbkdf2-check-password
                (babel:string-to-octets password :encoding :utf-8)
                hashed-password))
      account-id)))

(defun change-password (username old-password new-password)
  "Changes the password for the account associated with `username' from
`old-password' to `new-password'. Returns the account ID on success or nil on
failure."
  (when-let ((account-id (authenticate username old-password)))
    (let ((hashed-password (hash-password new-password)))
      (sqlite:execute-non-query
       *db*
       "update accounts set password = ? where account_id = ?" hashed-password account-id)
      account-id)))

(defun load-finished-quests (avatar-id)
  (apply #'sethash* (make-hash-table)
         (loop
           for (quest-id completion-time)
             in (sqlite:execute-to-list
                 *db*
                 "select quest_id, completion_time from finished_quests where avatar_id = ?"
                 avatar-id)
           nconc (list (from-string quest-id) completion-time))))

(defun load-tutorials-seen (avatar-id)
  (apply #'sethash* (make-hash-table)
         (loop
           for (tutorial-id)
             in (sqlite:execute-to-list
                 *db*
                 "select tutorial_id from tutorials_seen where avatar_id = ?"
                 avatar-id)
           nconc (list (from-string tutorial-id) t))))

(defun load-avatar (account-id)
  ;; TODO: also load aliases and settings
  (handler-case
      (sqlite:with-transaction *db*
        (multiple-value-bind (location avatar-data avatar-id)
            (sqlite:execute-one-row-m-v
             *db*
             "select location, avatar, avatar_id from avatars where account_id = ?"
             account-id)
          (let ((avatar (decode-entity (from-string avatar-data))))
            (setf (avatar-id avatar) avatar-id
                  (avatar-account-id avatar) account-id
                  (finished-quests avatar) (load-finished-quests avatar-id)
                  (tutorials-seen avatar) (load-tutorials-seen avatar-id))
            (with-attributes (inventory) avatar
              (setf inventory (sort inventory #'item<)))
            (values avatar (from-string location)))))
    (sqlite:sqlite-error (err)
      (format-log :warning "cannot load avatar: ~s" err)
      nil)))

(defun save-aliases (account-id aliases)
  (let ((data (to-string aliases)))
    (sqlite:execute-non-query
     *db*
     "update avatars set aliases = ? where account_id = ?" data account-id)))

(defun save-settings (account-id settings)
  (let ((data (to-string settings)))
    (sqlite:execute-non-query
     *db*
     "update avatars set settings = ? where account_id = ?" data account-id)))

(defun reset-tutorials (avatar-id)
  (sqlite:execute-non-query
   *db*
   "delete from tutorials_seen where avatar_id = ?" avatar-id))

(defun update-dirty-tutorials (avatar)
  (with-slots (avatar-id dirty-tutorials) avatar
    (dolist (tutorial-id dirty-tutorials)
      (sqlite:execute-non-query
       *db*
       "insert or replace into tutorials_seen (avatar_id, tutorial_id) values (?, ?)"
       avatar-id (to-string tutorial-id)))))

(defun update-dirty-quests (avatar)
  (with-slots (avatar-id dirty-quests finished-quests) avatar
    (loop for label in dirty-quests do
      (let ((completion-time (gethash label finished-quests)))
        (sqlite:execute-non-query
         *db*
         "insert or replace into finished_quests (avatar_id, quest_id, completion_time) values (?, ?, ?)"
         avatar-id (to-string label) completion-time)))))

(defun save-avatar (avatar location)
  (handler-case
      (sqlite:with-transaction *db*
        (sqlite:execute-non-query
         *db*
         "update avatars set location = ?, avatar = ? where avatar_id = ?"
         (to-string location) (to-string (encode-entity avatar)) (avatar-id avatar))
        (update-dirty-tutorials avatar)
        (update-dirty-quests avatar)
        (setf (dirty-quests avatar) nil
              (dirty-tutorials avatar) nil)
        t)
    (sqlite:sqlite-error (err)
      (format-log :warning "cannot save avatar: ~s" err)
      nil)))
