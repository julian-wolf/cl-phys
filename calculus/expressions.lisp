(in-package :cl-phys.calculus)

(define-condition bad-math-expression-error (error)
  ((message :initarg :message :reader message)
   (expression :initarg :expression :reader expression)))

(defun unnest (expression)
  (if (and (listp (car expression))
	   (eq (length expression) 1))
      (car expression)
      expression))

;;; functions to make arithmetic expressions
(defmacro make-make-expression-body (combiner arg-list identity-value
				     flattener-function)
  `(labels ((remove-nested-expressions (arg-list)
	      (let ((arg (car arg-list)))
		(cond
		  ((null arg)
		   nil)
		  ((and (listp arg)
			(eq (car arg)
			    ,combiner))
		   (append (cdr arg)
			   (remove-nested-expressions
			    (cdr arg-list))))
		  (t
		   (cons arg (remove-nested-expressions
			      (cdr arg-list)))))))
	    (find-equivalent-expressions (arg-list expr-list)
	      (let ((arg (car arg-list)))
		(cond
		  ((null arg)
		   expr-list)
		  ((variable-or-expression-p arg)
		   (let ((existing-expr
			  (find arg expr-list
				:test (lambda (val pair)
					(same-variable-or-expression-p
					 val (car pair))))))
			 (find-equivalent-expressions
			  (cdr arg-list)
			  (if existing-expr
			      (cons (list arg (1+ (cadr existing-expr)))
				    (remove arg expr-list
					    :test (lambda (val pair)
						    (same-variable-or-expression-p
						     val (car pair)))))
			      (cons (list arg 1)
				    expr-list)))))
		  (t
		   (find-equivalent-expressions (cdr arg-list)
						(cons (list arg 1)
						      expr-list))))))
	    (remove-equivalent-args (expr-list)
	      (let ((arg-pair (car expr-list)))
		(cond
		  ((null arg-pair)
		   nil)
		  ((eq (cadr arg-pair) 1)
		   (cons (car arg-pair)
			 (remove-equivalent-args (cdr expr-list))))
		  (t
		   (cons (funcall ,flattener-function
				  (car arg-pair)
				  (cadr arg-pair))
			 (remove-equivalent-args (cdr expr-list)))))))
	    (remove-equivalent-expressions (arg-list)
	      (remove-equivalent-args (find-equivalent-expressions
				     arg-list nil))))
     (let* ((arg-list (remove-equivalent-expressions
		       (remove-nested-expressions (unnest ,arg-list))))
	    (numbers (remove-if-not #'number-p arg-list))
	    (non-numbers (remove-if #'number-p arg-list))
	    (number-value (apply ,combiner numbers))
	    (new-arg-list (if (eq number-value ,identity-value)
			      non-numbers
			      (cons number-value non-numbers))))
       (case (length new-arg-list)
	 (0 ,identity-value)
	 (1 (car new-arg-list))
	 (otherwise (cons ,combiner new-arg-list))))))

;; basic arithmetic
(defun make-sum (&rest expressions)
  (make-make-expression-body '+ expressions 0 #'make-product))

(defun make-product (&rest expressions)
  (if (find 0 (unnest expressions))
      0
      (make-make-expression-body '* expressions 1 #'make-pow)))

(defun make-difference (&rest expressions)
  (let ((expressions (unnest expressions)))
    (make-sum (car expressions)
	      (make-product -1 (make-sum (cdr expressions))))))

(defun make-quotient (&rest expressions)
  (let ((expressions (unnest expressions)))
    (make-product (car expressions)
		  (make-pow (make-product (cdr expressions)) -1))))

;; trigonometry
(defun make-sin (expression)
  (list 'sin expression))

(defun make-cos (expression)
  (list 'cos expression))

(defun make-tan (expression)
  (list 'tan expression))

;; other functions
(defun make-exp (expression)
  (list 'exp expression))

(defun make-log (expression)
  (list 'log expression))

(defun make-pow (base power)
  (list 'expt base power))


;;; predicates to test the types of arithmetic expressions
(defun number-p (var)
  (numberp var))

(defun variable-p (var)
  (symbolp var))

(defun same-variable-p (lhs rhs)
  (and (variable-p lhs)
       (variable-p rhs)
       (equal lhs rhs)))

(defparameter *all-predicate-functions* nil)

(defmacro make-expression-predicate (name function-name)
  `(progn
     (defun ,name (expr)
       (and (listp expr)
	    (eq (car expr) ,function-name)))
     (push ,function-name *all-predicate-functions*)))

;; basic arithmetic
(make-expression-predicate sum-p '+)
(make-expression-predicate product-p '*)
(make-expression-predicate difference-p '-)
(make-expression-predicate quotient-p '/)

;; trigonometry
(make-expression-predicate sin-p 'sin)
(make-expression-predicate cos-p 'cos)
(make-expression-predicate tan-p 'tan)

;; other things
(make-expression-predicate exp-p 'exp)
(make-expression-predicate log-p 'log)
(make-expression-predicate pow-p 'expt)

(defun arithmetic-expression-p (expr)
  (and (listp expr)
       (find (car expr) *all-predicate-functions*)))

(defun same-arithmetic-expression-p (lhs rhs)
  (and (arithmetic-expression-p lhs)
       (arithmetic-expression-p rhs)
       (equal lhs rhs)))

(defun variable-or-expression-p (expr)
  (or (variable-p expr)
      (arithmetic-expression-p expr)))

(defun same-variable-or-expression-p (lhs rhs)
  (or (same-variable-p lhs rhs)
      (same-arithmetic-expression-p lhs rhs)))

;;; functions to get the arguments of arithmetic expressions
(defmacro make-expression-args-body (test element-accessor fail-message)
  `(if (apply ,test (list expr))
       (apply ,element-accessor (list expr))
       (error 'bad-math-expression-error
	      :message ,fail-message
	      :expression expr)))

;; basic arithmetic
(defun summands (expr)
  (make-expression-args-body #'sum-p #'cdr
			     "Expression is not a sum."))

(defun multiplicands (expr)
  (make-expression-args-body #'product-p #'cdr
			     "Expression is not a product."))

;; every expression that takes a single argument
(defun argument (expr)
  (make-expression-args-body #'(lambda (expr)
				  (or (sin-p expr)
				      (cos-p expr)
				      (tan-p expr)
				      (exp-p expr)
				      (log-p expr)))
			     #'cadr
			     "Expression does not take a singular argument."))

;; exponentiation (powers)
(defun base (expr)
  (make-expression-args-body #'pow-p #'cadr
			     "Only pow expressions have well-defined bases."))

(defun power (expr)
  (make-expression-args-body #'pow-p #'caddr
			     "Only pow expressions have well-defined powers."))

(defun get-variables (expr)
    (cond
      ((variable-p (car expr))
       (cons (car expr)
	     (get-variables (cdr expr))))
      ((arithmetic-expression-p (car expr))
       (append (get-variables (car expr))
	       (get-variables (cdr expr))))
      (t
       (get-variables (cdr expr)))))

(defun expression->function (expr)
  (labels ((convert-expression (expr)
	     (cond
	       ((null expr)
		(values nil nil))
	       ((arithmetic-expression-p expr)
		(multiple-value-bind
		  (converted-expression variable-list)
	          (convert-expression (cdr expr))
		  (values (cons (car expr)
				converted-expression)
			  variable-list)))
	       ((listp expr)
		(let ((arg (car expr)))
		  (multiple-value-bind
		    (converted-expression variable-list)
		    (convert-expression (cdr expr))
		    (cond
		      ((null arg)
		       (values nil nil))
		      ((number-p arg)
		       (values (cons arg converted-expression)
			       variable-list))
		      ((variable-p arg)
		       (let ((interned-arg (intern (symbol-name arg))))
			 (values (cons interned-arg converted-expression)
				 (cons interned-arg variable-list))))
		      ((arithmetic-expression-p arg)
		       (multiple-value-bind
			 (converted-expression-arg variable-list-arg)
			 (convert-expression arg)
			 (values (cons converted-expression-arg
				       converted-expression)
				 (append variable-list-arg
					 variable-list))))
		      (t
		       (error "Expression not of supported list type."))))))
	       ((variable-p expr)
		(let ((interned-expr (intern (symbol-name expr))))
		  (values interned-expr (cons interned-expr nil))))
	       ((number-p expr)
		(values expr nil))
	       (t
		(error "Expression not of supported type. Uh oh!")))))
    (multiple-value-bind
      (converted-expression variable-list)
      (convert-expression expr)
      (coerce `(lambda ,(cons '&key
			      (mapcar (lambda (var)
					(list var 0))
				      (remove-duplicates variable-list)))
		 ,converted-expression) ; TODO: account for unexpected variables
	      'function))))
