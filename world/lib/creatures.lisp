(in-package :jade.lib)

(defentity creature ()
  (:brief "a creature"))

(defentity humanoid (creature)
  (:brief "a humanoid")

  (:when-talk (actor self topic)
    (show actor "~a has nothing to say to you."
          (describe-brief self :article :definite :capitalize t))))
