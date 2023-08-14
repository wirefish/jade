(defpackage :jade
  (:documentation "Core server functionality.")
  (:use :cl :alexandria-2)
  (:import-from :cl-async :with-delay)
  (:shadow :inspect)
  (:export #:strcat
           #:string-starts-with
           #:string-ends-with
           #:?
           #:->
           #:bind
           #:with-delay
           #:uniform-random

           #:entity
           #:entity-id
           #:entity-label
           #:entity-proto
           #:entity-isa
           #:defentity
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
           #:self
           #:&quest
           #:&race
           #:&dir

           #:item
           #:defmaterial
           #:give
           #:contains-isa

           #:location
           #:deflocation
           #:spawn-entity
           #:spawn-unique-entity
           #:make-exit
           #:direction-opposite

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

           #:maybe-show-tutorial

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
  :jade.arwyck)
