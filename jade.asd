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

             (:file "config")
             (:file "entity")
             (:file "activity")
             (:file "behavior")
             (:file "match")
             (:file "item")
             (:file "location")
             (:file "combat")
             (:file "avatar")
             (:file "database")
             (:file "map")
             (:file "client")
             (:file "command")

             (:file "help")
             (:file "tutorial")
             (:file "offer")
             (:file "inspect")
             (:file "inventory")
             (:file "vendor")
             (:file "skill")
             (:file "gather")
             (:file "move")
             (:file "talk")
             (:file "quest")
             (:file "meditate")
             (:file "attack")
             (:file "use")

             (:file "server")
             ))

   (:module "world" :components
            ((:module "lib" :components
                      ((:file "creatures")
                       (:file "currency")
                       (:file "clothing")
                       (:file "weapons")
                       (:file "portals")
                       (:file "vehicles")
                       (:file "mining")
                       (:file "avatar")
                       ))
             (:file "isle-of-dawn")
             (:file "arwyck")
             ))))
