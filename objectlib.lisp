(defvar *class-list* ())

(defmacro defineclass (classname &body body)
  (let ((b (loop for form in body
	      append (macroexpand form))))
  `(setf *class-list*
	 (cons '(,classname . ,b) *class-list*))))

(defmacro vars (&rest rest)
  (let* ((pairs (pair rest))
	 (varlist (loop for p in pairs
		     collect (car p))))
    (append `((variables ,varlist)) pairs)))

(defmacro this (var)
  `(cdr (assoc ',var (cdr self))))

(defmacro definemethod (name args &body body)
  (let* ((arglist (cons 'self args))
	 (f (fn arglist body)))
    (list (cons name f))))

(defun fn (args body)
  (eval (append '(lambda) (list args) body)))

;;;Pair function and split macro taken from 
;;;"Land of Lisp" by Conrad Barski
(defun pair (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

(defmacro split (val yes no)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
	   (let ((head (car ,g))
		 (tail (cdr ,g)))
	     ,yes)
	   ,no))))

(defun new (objtype &rest args)
  (let ((prototype (copy-alist (assoc objtype *class-list*))))
    (when prototype
      (let* ((vars (assoc 'variables (cdr prototype)))
	     (obj (cons (car prototype) (loop for var in (cadr vars)
					   collect (assoc var (cdr prototype))))))
	obj))))

(defun object-type (obj)
  (car obj))

(defun call (function-name object-name &rest args)
  (let* ((o (assoc (object-type object-name) *class-list*))
	 (f (cdr (assoc function-name (cdr o)))))
    (apply f object-name args)))

(defun clone (object)
  (copy-alist object))
