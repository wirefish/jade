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

           #:entity
           #:entity-id
           #:entity-label
           #:entity-proto
           #:entity-isa
           #:defentity
           #:clone
           #:clone*
           #:with-attributes
           #:describe-brief

           #:defbehaviors
           #:disallow-action
           #:call-next-handler
           #:self
           #:&quest
           #:&race

           #:item
           #:defmaterial
           #:give
           #:contains-isa

           #:location
           #:deflocation
           #:spawn
           #:spawn-if-missing

           #:avatar
           #:change-race
           #:change-name

           #:show
           #:show-notice
           #:tell

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
