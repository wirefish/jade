(defsystem "jade"
  :description "jade: a multiuser text-based role-playing game server"
  :version "0.0.1"
  :author "Craig Becker <craig@wirefish.com>"
  :license "MIT"
  :depends-on ("cl-ppcre" "cl-async" "cl-base64" "ironclad" "sqlite")
  :serial t
  :build-operation "program-op"
  :build-pathname "build/jade-server"
  :entry-point "jade::run-server"
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
             (:file "generator")
             (:file "activity")
             (:file "behavior")
             (:file "message")
             (:file "match")
             (:file "item")
             (:file "resource")
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
             (:file "recall")
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
             (:file "loot")

             (:file "server")
             ))

   (:module "world" :components
            ((:module "lib" :components
                      ((:file "definitions")
                       (:file "currency")
                       (:file "clothing")
                       (:file "armor")
                       (:file "weapons")
                       (:file "portals")
                       (:file "vehicles")
                       (:file "botany")
                       (:file "logging")
                       (:file "mining")
                       (:file "skinning")
                       (:file "creatures")
                       (:file "avatar")
                       ))
             (:file "isle-of-dawn")
             (:file "arwyck")
             (:file "silverwood")
             (:file "copper-mine")
             ))))
