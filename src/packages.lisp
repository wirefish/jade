(defpackage :jade
  (:documentation "Core server functionality.")
  (:use :cl :alexandria-2)
  (:import-from :cl-async :with-delay)
  (:shadow :inspect)
  (:export #:symbol-value-or-nil
           #:strcat
           #:string-starts-with
           #:string-ends-with
           #:?
           #:->
           #:bind
           #:with-delay

           #:round-random
           #:random-integer
           #:random-float

           #:parse-noun
           #:parse-verb

           #:entity
           #:entity-id
           #:entity-label
           #:entity-proto
           #:entity-isa
           #:defentity
           #:defbehavior
           #:clone-entity
           #:with-attributes
           #:describe-brief

           #:+miniscule+
           #:+tiny+
           #:+small+
           #:+medium+
           #:+large+
           #:+huge+
           #:+gigantic+

           #:defbehaviors
           #:disallow-action
           #:call-next-handler
           #:observe-event
           #:show-observers
           #:self
           #:&quest
           #:&race
           #:&dir

           #:item
           #:receive
           #:contains-isa
           #:find-item-isa
           #:remove-item-isa

           #:location
           #:deflocation
           #:spawn-entity
           #:spawn-unique-entity
           #:make-exit
           #:exit-dir
           #:exit-dest
           #:direction-opposite
           #:traverse-portal
           #:find-exit

           #:avatar
           #:change-race
           #:change-name
           #:for-avatars-in
           #:for-avatars-near

           #:show
           #:show-notice
           #:show-map
           #:tell
           #:announce
           #:show-near

           #:maybe-show-tutorial

           #:defskill

           #:defquest
           #:offer-quest
           #:advance-quest
           #:quest-phase
           ))

(defpackage :jade.lib
  (:documentation "General game definitions not tied to a particular location.")
  (:use :cl :alexandria-2 :jade))

;;; Define a package associated with each region of the game world.

(defmacro jade/defregionpackages (&body names)
  `(progn
     ,@(loop for name in names
             collect `(defpackage ,name
                        (:use :cl :alexandria-2 :jade :jade.lib)))))

(jade/defregionpackages
  :jade.isle-of-dawn
  :jade.arwyck
  :jade.silverwood
  :jade.mistmarsh
  :jade.copper-mine)
