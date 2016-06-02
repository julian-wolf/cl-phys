(in-package :cl-phys.calculus)

(defun differentiate-analytic (expr var)
  "Try to analytically differentiate an
expression expr with respect to variable var."
  (cond
    ((number-p expr)
     0)
    ((variable-p expr)
     (if (same-variable-p expr var)
	 1
	 0))
    ((sum-p expr)
     (make-sum (mapcar (lambda (summand)
			 (differentiate-analytic summand var))
		       (summands expr))))
     ((product-p expr)
     (let ((multiplicands (multiplicands expr)))
       (make-sum (loop for multiplicand in multiplicands
		    for i from 0
		    collecting
		      (make-product
		       (cons (differentiate-analytic multiplicand  var)
			     (loop for multiplier in multiplicands
				for j from 0
				unless (= i j)
				collect multiplier)))))))
    ((sin-p expr)
     (make-product (differentiate-analytic (argument expr) var)
		   (make-cos (argument expr))))
    ((cos-p expr)
     (make-product -1
		   (differentiate-analytic (argument expr) var)
		   (make-sin (argument expr))))
    ((tan-p expr)
     (make-quotient (differentiate-analytic (argument expr) var)
		    (make-pow (make-cos (argument expr)) 2)))
    ((exp-p expr)
     (make-product (differentiate-analytic (argument expr) var)
		   expr))
    ((log-p expr)
     (make-quotient (differentiate-analytic (argument expr) var)
		    (argument expr)))
    ((pow-p expr)
     (let* ((base (base expr))
	    (power (power expr))
	    (power-derivative (differentiate-analytic power var))
	    (prefactor (make-pow base (make-difference (list power 1))))
	    (first-term (make-product power
				      (differentiate-analytic base var)))
	    (second-term (if (eq power-derivative 0)
			     0
			     (make-product base (make-log base)))))
       (make-product prefactor (make-sum first-term second-term))))
    (t (error 'bad-math-expression-error
	      :message "Expression not differentiable."
	      :expression expr))))

(defun eval-derivative-analytic (expr var values-plist)
  "Analytically evaluate the derivative of an expression expr
with respect to the variable var at the point described by
values-plist."
  (apply (expression->function
	  (differentiate-analytic expr var))
	 values-plist))

(defun eval-derivative-numeric (expr var values-plist
				&optional (epsilon 1.0d-7))
  "Numerically evaluate the derivative of an expression expr
with respect to the variable var at the point described by
values-plist."
  (let ((expr-function (expression->function expr))
	(var-symbol (find-symbol (string var))))
    (labels ((increment-var (values-plist)
	       ;; Increment the variable var by epsilon.
	       (if (null values-plist)
		   nil
		   (let ((key (car values-plist))
			 (val (cadr values-plist)))
		     (cons key
			   (cons (if (eq (find-symbol (string key))
					 var-symbol)
				     (+ val epsilon)
				     val)
				 (increment-var
				  (cddr values-plist))))))))
      (/ (- (apply expr-function (increment-var values-plist))
	    (apply expr-function values-plist))
	 epsilon))))

