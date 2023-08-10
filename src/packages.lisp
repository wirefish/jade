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

           #:entity
           #:entity-id
           #:entity-label
           #:entity-proto
           #:entity-isa
           #:clone-entity
           #:defentity
           #:with-attributes

           #:defbehaviors
           #:disallow-action
           #:call-next-handler
           #:self

           #:item
           #:defmaterial

           #:location
           #:deflocation

           #:avatar

           #:show
           #:show-notice
           #:tell

           #:defquest
           #:offer-quest
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
