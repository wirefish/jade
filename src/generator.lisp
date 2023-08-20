(in-package :jade)

;;; Each rule is a list (probability what . forms). If the rule is selected
;;; based on its probability, then what and forms are evaluated. If what is a
;;; function, calls it with the evaluated forms as arguments. If what is an
;;; entity, calls `clone-entity' with it and the evaluated forms as arguments.

(defun make-generator (rules)
  (lambda ()
    (loop for (probability what . forms) in rules
          when (< (random 1.0) probability)
            collect (let ((what (eval what))
                          (args (mapcar #'eval forms)))
                      (typecase what
                        (function (apply what args))
                        (entity (apply #'clone-entity what args)))))))

(defmacro defgenerator (name rules)
  `(progn
     (setf (fdefinition ',name) (make-generator ',rules))
     ',name))
