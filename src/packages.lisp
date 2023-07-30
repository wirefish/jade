(defpackage :jade
  (:documentation "Core server functionality.")
  (:use :cl)
  (:import-from :cl-async :with-delay)
  (:shadow :inspect)
  (:export #:entity
           #:entity-id
           #:entity-label
           #:entity-proto
           #:entity-isa
           #:clone-entity
           #:defentity
           #:with-attributes

           #:defbehaviors

           #:item
           #:defmaterial

           #:deflocation

           #:if-let
           #:if-let*
           #:when-let
           #:when-let*
           #:strcat
           #:string-starts-with
           #:string-ends-with
           #:?
           #:->
           #:bind))

(defpackage :jade.lib
  (:documentation "General game definitions not tied to a particular location.")
  (:use :cl :jade))

;;; Define a package associated with each region of the game world.

(defmacro jade/defregionpackages (&body names)
  `(progn
     ,@(loop for name in names
             collect `(defpackage ,name
                        (:use :cl :jade :jade.lib)))))

(jade/defregionpackages
  :jade.isle-of-dawn
  :jade.arwyck)
