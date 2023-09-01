(in-package :jade)

;;; Generic utilities.

(defun symbol-value-or-nil (symbol)
  (and (boundp symbol) (symbol-value symbol)))

(defun symbol-value-as (type symbol &optional (default nil default-present))
  (if (boundp symbol)
      (let ((value (symbol-value symbol)))
        (or (when (typep value type) value)
            (if default-present
                default
                (error "value of symbol ~a does not have required type ~a" symbol type))))
      (if default-present
          default
          (error "symbol ~a is unbound" symbol))))

;;; String utilities.

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun string-starts-with (string prefix)
  (and (<= (length prefix) (length string))
       (string= string prefix :end1 (length prefix))))

(defun string-ends-with (string suffix)
  (let ((start (- (length string) (length suffix))))
    (and (>= start 0) (string= string suffix :start1 start))))

(defun to-string (value)
  (with-output-to-string (s)
    (write value :stream s)))

(defun from-string (s)
  (with-input-from-string (stream s)
    (read stream)))

;;; List utilities.

(defun mapleaves (fn tree)
  "Calls unary function `fn' to transform each leaf node of `tree', returning a
new tree with the same structure."
  (labels ((recurse (node)
             (etypecase node
               (atom (funcall fn node))
               (list (mapcar #'recurse node)))))
    (recurse tree)))

(defun tree-contains (value tree &key (test #'eql))
  "Returns true if `tree' contains a leaf node that matches `value' subject to
`test'."
    (labels ((recurse (node)
               (etypecase node
                 (atom (when (funcall test node value)
                         (return-from tree-contains t)))
                 (list (dolist (child node) (recurse child))))))
      (recurse tree)))

(defun find-all-in-tree-if (pred tree)
  "Returns all nodes in `tree' that satisfy `pred', in depth-first order."
  (let (matches)
    (labels ((recurse (node)
               (when (funcall pred node)
                 (push node matches))
               (when (listp node)
                 (dolist (child node) (recurse child)))))
      (recurse tree)
      (nreverse matches))))

(defun split-list (list n)
  "Splits `list' into two lists and returns the results as two values. If `n' is
non-negative, the first value contains the first `n' elements of `list' and the
second value contains any remaining elements. If `n' is negative, the second
value contains the last `-n' elements and the first value contains any remaining
elements."
  (if (>= n 0)
      (values (subseq list 0 n) (subseq list n))
      (values (butlast list (- n)) (last list (- n)))))

(defun map-plist-values (fn plist)
  "Returns a new plist with the same keys as `plist' in the same order, but
whose values are the result of applying `fn' to each."
  (loop for (k v) on plist by #'cddr
        nconc (list k (funcall fn v))))

(defun insert-sorted (item list predicate)
  "Returns the result of modifying `list' to insert `item' before the first
element `x' for which (predicate item x) returns true."
  (cond
    ((null list) (list item))
    ((funcall predicate item (car list))
     (cons item list))
    (t
     (loop for tail on list do
       (when (or (null (cdr tail)) (funcall predicate item (second tail)))
         (setf (cdr tail) (cons item (cdr tail)))
         (return list))))))

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

(defun merge-hash-tables (output fn &rest inputs)
  "Merges the entries from each of `inputs' into `output', calling `fn' to combine
two values for the same key. Returns `output'."
  (loop for input in inputs do
    (loop for key being the hash-keys in input using (hash-value value) do
      (multiple-value-bind (output-value found) (gethash key output)
        (sethash key output (if found (funcall fn output-value value) value)))))
  output)

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

;;; Enable #`(...) syntax to define unary lambda expression. Any occurence of %
;;; in the body is replaced with the parameter name.

(defun |#`-reader| (stream char arg)
  (declare (ignore char arg))
  (let* ((param (gensym))
         (body (mapleaves (lambda (x) (if (eq x '%) param x)) (read stream t nil t))))
    `(lambda (,param)
       ,body)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;;; Generic access to an element of a (possibly nested) data structure.

(defgeneric ? (obj key &rest keys)
  (:method :around (obj key &rest keys)
    (reduce #'? keys :initial-value (call-next-method obj key)))
  (:method (obj key &rest keys)
    (declare (ignore keys))
    (error "invalid element access"))
  (:method ((obj null) key &rest keys)
    (declare (ignore keys))
    nil)
  (:method ((obj list) (key integer) &rest keys)
    (declare (ignore keys))
    (nth (if (minusp key) (+ (length obj) key) key) obj))
  (:method ((obj vector) (key integer) &rest keys)
    (declare (ignore keys))
    (aref obj (if (minusp key) (+ (length obj) key) key)))
  (:method ((obj hash-table) key &rest keys)
    (declare (ignore keys))
    (gethash key obj))
  (:method ((obj standard-object) key &rest keys)
    (declare (ignore keys))
    (slot-value obj key))
  (:method (obj (key function) &rest keys)
    (declare (ignore keys))
    (funcall key obj)))

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
    (sethash key obj (first keys-and-value)))
  (:method ((obj standard-object) key &rest keys-and-value)
    (setf (slot-value obj key) (first keys-and-value))))

(defsetf ? generic-setf)

;;; Provide a closure-style name for the threading operator.

(defmacro -> (&rest forms)
  `(line-up-first ,@forms))

;;; More expressive binding.

(defvar *ignored-bindings* nil
  "A list of gensym'd symbols used as placeholders for ignored binding variables.")

(defun maybe-ignore (var)
  (typecase var
    (null (car (push (gensym "IGNORED") *ignored-bindings*)))
    (keyword (error "cannot bind value to keyword ~s" var))
    (symbol var)
    (otherwise (error "bound variable must be a symbol, not ~s" var))))

(defgeneric expand-binding (arg1 arg2 &rest args)
  (:method ((arg1 symbol) arg2 &rest args)
    (if args
        (let (*ignored-bindings*)
          `(multiple-value-bind (,(maybe-ignore arg1) ,(maybe-ignore arg2)
                                 ,@(mapcar #'maybe-ignore (butlast args)))
               ,@(last args)
             ,@(when *ignored-bindings*
                 `((declare (ignore ,@*ignored-bindings*))))))
        `(let ((,arg1 ,arg2)))))
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

;;; An allocator is a lambda that wraps a semaphore-style counter.

(defun make-allocator (limit)
  (let ((counter limit))
    (lambda (op)
      (case op
        (:acquire (when (> counter 0) (decf counter)))
        (:release (incf counter))
        (:value counter)))))

(defmacro defallocator (name limit)
  `(setf (fdefinition ',name) (make-allocator ,limit)))
