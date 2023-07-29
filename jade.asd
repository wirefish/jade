(defsystem "jade"
  :description "jade: a multiuser text-based role-playing game server"
  :version "0.0.1"
  :author "Craig Becker <craig@wirefish.com>"
  :license "MIT"
  :depends-on ("cl-ppcre" "cl-async" "cl-base64" "ironclad" "sqlite")
  :serial t
  :components
  ((:module "src" :components
            ((:file "packages")
             (:file "utility")
             (:file "text")
             (:file "entity")))))
