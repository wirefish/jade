(in-package :jade)

;;; Miscellaneous utilities.

(defmacro with-gensyms ((&rest names) &body body)
  "Provides gensyms for `names'."
  `(let ,(mapcar (lambda (name) `(,name (gensym))) names)
     ,@body))

;;; Flow control macros.

(defmacro when-let (bindings &body body)
  "Evaluates bindings as with `let' then executes `body' only if all bound values
are non-nil."
  `(let ,bindings
     (when (and ,@(mapcar #'first bindings))
       ,@body)))

(defmacro when-let* (bindings &body body)
  "Evaluates bindings as with `let*' then executes `body' only if all bound values
are non-nil."
  `(let* ,bindings
     (when (and ,@(mapcar #'first bindings))
       ,@body)))

(defmacro if-let (bindings then &optional else)
  "Evaluates bindings as with `let' and executes `then' if all bound values are
non-nil, or `else' otherwise."
  `(let ,bindings
     (if (and ,@(mapcar #'first bindings)) ,then ,else)))

(defmacro if-let* (bindings then &optional else)
  "Evalulates bindings as with `let*' and executes `then' if all bound values are
non-nil, or `else' otherwise."
  `(let* ,bindings
     (if (and ,@(mapcar #'first bindings)) ,then ,else)))

;;; String utilities.

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun string-starts-with (string prefix)
  (and (<= (length prefix) (length string))
       (string= string prefix :end1 (length prefix))))

(defun string-ends-with (string suffix)
  (let ((start (- (length string) (length suffix))))
    (and (>= start 0) (string= string suffix :start1 start))))

;;; List utilities.

(defun mapleaves (fn tree)
  "Calls unary function `fn' to transform each leaf node of `tree', returning a
new tree with the same structure."
  (labels ((recurse (node)
             (etypecase node
               (atom (funcall fn node))
               (list (mapcar #'recurse node)))))
    (recurse tree)))

(defun keep-leaves (fn tree)
  "Returns a list of the non-null results of calling unary function `fn' on each
leaf node of `tree'. The results are returned in depth-first order."
  (let (results)
    (labels ((recurse (node)
               (etypecase node
                 (atom (when-let ((x (funcall fn node))) (push x results)))
                 (list (dolist (child node) (recurse child))))))
      (recurse tree)
      (nreverse results))))

(defun split-list (list n)
  "Splits `list' into two lists and returns the results as two values. If `n' is
non-negative, the first value contains the first `n' elements of `list' and the
second value contains any remaining elements. If `n' is negative, the second
value contains the last `-n' elements and the first value contains any remaining
elements."
  (if (>= n 0)
      (values (subseq list 0 n) (subseq list n))
      (values (butlast list (- n)) (last list (- n)))))

(defun everyp (pred &rest lists)
  (if (position-if #'null lists)
      t
      (when-let ((x (apply pred (mapcar #'car lists))))
        (apply #'everyp pred (mapcar #'cdr lists)))))

;;; Hash table utilities.

(declaim (inline sethash))
(defun sethash (key hash-table value)
  "Sets `value' for `key' in `hash-table'."
  (setf (gethash key hash-table) value))

(defun sethash* (hash-table &rest keys-and-values)
  (loop for (key value) on keys-and-values by #'cddr do
    (sethash key hash-table value))
  hash-table)

(defun gethash* (hash-table &rest keys)
  (apply #'values (loop for key in keys collect (gethash key hash-table))))

(declaim (inline pophash))
(defun pophash (key hash-table &optional default)
  "Removes the value associated with `key' from `hash-table' and returns it. If
there was no such value, returns `default'."
  (prog1 (gethash key hash-table default)
    (remhash key hash-table)))

(defun pophash* (hash-table &rest keys)
  (apply #'values (loop for key in keys collect (pophash key hash-table))))

;;; Enable #h(...) syntax for hash-table literals. If the number of elements is
;;; odd, the first is a list of arguments to pass to `make-hash-table'.
;;; Subsequent elements are alternating keys and values.

(defun |#h-reader| (stream char arg)
  (declare (ignore char arg))
  (read-char stream)
  (let* ((forms (read-delimited-list #\) stream t))
         (args (when (oddp (length forms)) (car forms)))
         (keys-values (if args (cdr forms) forms))
         (table (gensym)))
    `(let ((,table (make-hash-table ,@args)))
       (sethash* ,table ,@keys-values))))

(set-dispatch-macro-character #\# #\h #'|#h-reader|)

;;; Enable #`(...) syntax for a lambda with positional arguments. Each argument
;;; is denoted by %N for N >= 1. (% is a synonym for %1.) The number of expected
;;; arguments is the highest value seen for N.

(defun |#`-reader| (stream char arg)
  (declare (ignore char arg))
  (labels ((arg-position (atom)
             (when (eq (type-of atom) 'symbol)
               (let ((name (symbol-name atom)))
                 (when (eql (char name 0) #\%)
                   (if (= (length name) 1)
                       1
                       (parse-integer name :start 1)))))))
    (let* ((body (read stream t nil t))
           (nargs (apply #'max (keep-leaves #'arg-position body)))
           (arg-names (loop repeat nargs collect (gensym))))
      `(lambda ,arg-names
         (declare (ignorable ,@arg-names))
         ,(mapleaves (lambda (node)
                       (if-let ((pos (arg-position node)))
                         (nth (1- pos) arg-names)
                         node))
                     body)))))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;;; Generic access to an element of a (possibly nested) data structure.

(defgeneric ? (obj key &rest keys)
  (:method :around (obj key &rest keys)
    (reduce #'? keys :initial-value (call-next-method obj key)))
  (:method (obj key &rest keys)
    (declare (ignore keys))
    (error "invalid container"))
  (:method ((obj list) key &rest keys)
    (declare (ignore keys))
    (nth (if (minusp key) (+ (length obj) key) key) obj))
  (:method ((obj vector) key &rest keys)
    (declare (ignore keys))
    (aref obj (if (minusp key) (+ (length obj) key) key)))
  (:method ((obj hash-table) key &rest keys)
    (declare (ignore keys))
    (gethash key obj))
  (:method ((obj standard-object) key &rest keys)
    (declare (ignore keys))
    (slot-value obj key)))

(defgeneric generic-setf (obj key &rest keys-and-value)
  (:method :around (obj key &rest keys-and-value)
    (if (null (cdr keys-and-value))
        (call-next-method)
        (multiple-value-bind (get-keys set-key-and-value)
            (split-list keys-and-value -2)
          (apply #'generic-setf
                 (apply #'? obj key get-keys)
                 set-key-and-value))))
  (:method ((obj list) key &rest keys-and-value)
    (setf (nth key obj) (first keys-and-value)))
  (:method ((obj hash-table) key &rest keys-and-value)
    (sethash key obj (first keys-and-value))))

(defsetf ? generic-setf)

;;; Closure-like threading operator.

(defmacro -> (expr &rest forms)
  (if (first forms)
      (let* ((form (first forms))
             (threaded (if (listp form)
                           (if (member '% form)
                               `(funcall (lambda (%) ,form) ,expr)
                               `(,(first form) ,expr ,@(rest form)))
                           `(,form ,expr))))
        `(-> ,threaded ,@(rest forms)))
      expr))

;;; More expressive binding.

(defvar *ignored-bindings* nil
  "A list of gensym'd symbols used as placeholders for ignored binding variables.")

(defun maybe-ignore (var)
  (typecase var
    (null (car (push (gensym "IGNORED") *ignored-bindings*)))
    (keyword (error "cannot use bind value to keyword: ~a" var))
    (symbol var)
    (otherwise (error "invalid variable in binding: ~a" var))))

(defgeneric expand-binding (arg1 arg2 &rest args)
  (:method ((arg1 symbol) arg2 &rest args)
    (if args
        (let (*ignored-bindings*)
          `(multiple-value-bind (,(maybe-ignore arg1) ,(maybe-ignore arg2)
                                 ,@(mapcar #'maybe-ignore (butlast args)))
               ,@(last args)
             ,@(when *ignored-bindings*
                 `((declare (ignore ,@*ignored-bindings*))))))
        `(let (,arg1 ,arg2))))
  (:method ((arg1 list) (arg2 (eql '&slots)) &rest args)
    `(with-slots ,arg1 ,(first args)))
  (:method ((arg1 list) arg2 &rest args)
    (declare (ignore args))
    (let (*ignored-bindings*)
      `(destructuring-bind ,(mapleaves #'maybe-ignore arg1) ,arg2
         ,@(when *ignored-bindings*
             `((declare (ignore ,@*ignored-bindings*))))))))

(defmacro bind ((&rest bindings) &body body)
  "`bind' is like `let*' but supports additional binding forms:

  (var expr)  ; as let*
  (var1 ... :values expr)  ; uses multiple-value-bind
  (list expr)  ; uses destructuring-bind
  (list &slots obj)  ; uses with-slots on `obj'

A variable name of `nil' can be used to indicate a binding that should be
ignored. Bindings that declare a variable without an explicit value are not
supported."
  (let ((result body))
    (dolist (binding (reverse bindings))
      (setf result `((,@(apply #'expand-binding binding) ,@result))))
    (first result)))

;;; A simple FIFO queue.

(defstruct queue
  (head nil)
  (tail nil))

(defun queue-empty (queue)
  (null (queue-head queue)))

(defun queue-push (item queue)
  "Adds `item' to the back of `queue'."
  (let ((cell (cons item nil)))
    (if (queue-tail queue)
        (setf (cdr (queue-tail queue)) cell
              (queue-tail queue) cell)
        (setf (queue-head queue) cell (queue-tail queue) cell))))

(defun queue-pop (queue)
  "Removes and returns the item at the front of `queue'. The second return value
is t when a value was popped or nil otherwise."
  (if (queue-head queue)
      (let ((item (first (queue-head queue))))
        (setf (queue-head queue) (rest (queue-head queue)))
        (when (null (queue-head queue))
          (setf (queue-tail queue) nil))
        (values item t))
      (values nil nil)))
