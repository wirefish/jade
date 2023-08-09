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
             (:file "buffer")
             (:file "queue")
             (:file "random")
             (:file "logging")
             (:file "json")
             (:file "text")
             (:file "http")
             (:file "websocket")

             (:file "entity")
             (:file "behavior")
             (:file "item")
             (:file "location")
             (:file "avatar")
             (:file "match")
             (:file "config")
             (:file "database")
             (:file "map")
             (:file "client")
             (:file "command")

             (:file "help")
             (:file "tutorial")
             (:file "inspect")
             (:file "inventory")
             (:file "move")
             (:file "talk")

             (:file "server")
             ))

   (:module "world" :components
            ((:module "lib" :components
                      ((:file "avatar")
                       (:file "materials")
                       ))
             (:file "isle-of-dawn")
             ))))
