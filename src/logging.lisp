(in-package :jade)

(defvar *log-stream* t)

(defvar *log-levels* #(:debug :info :warning :error))

(defparameter *min-log-level* :info)

(defun format-log (level control-string &rest args)
  (when (>= (position level *log-levels*) (position *min-log-level* *log-levels*))
    (multiple-value-bind (second minute hour date month) (get-decoded-time)
      (format *log-stream* "~c ~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~?~%"
              (char (string level) 0) month date hour minute second control-string args))))

(defmacro log-run-time (level label &body body)
  (with-gensyms (timer)
    `(let ((,timer (get-internal-run-time)))
       (prog1
           (progn ,@body)
         (format-log ,level "~a took ~,6f sec" ,label
                     (/ (- (get-internal-run-time) ,timer)
                        internal-time-units-per-second))))))
