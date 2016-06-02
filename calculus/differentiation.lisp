(in-package :cl-phys.calculus)

(defun differentiate-analytic (expr var)
  "Try to analytically differentiate an
expression expr with respect to variable var."
  (cond
    ((number-p expr) 0)
    ((variable-p expr)
     (if (same-variable-p expr var)
	 1
	 0))
    ((sum-p expr)
     (make-sum (loop for summand in (summands expr)
		  collecting (differentiate-analytic summand var))))
    ((product-p expr)
     (let ((multiplicands (multiplicands expr)))
       (make-sum (loop for multiplicand in multiplicands
		    for i from 0
		    collecting
		      (make-product (cons (differentiate-analytic multiplicand
								  var)
					  (loop for multiplier in multiplicands
					     for j from 0
					     unless (= i j)
					     collecting multiplier)))))))
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
  (apply (expression->function
	  (differentiate-analytic expr var))
	 values-plist))

(defun eval-derivative-numeric (expr var values-plist
				&optional (epsilon 1.0d-6))
  (let ((expr-function (expression->function expr)))
    (/ (- (apply expr-function (loop with at-val = nil
				  for val in values-plist
				  if at-val
				  collect (+ val epsilon) and
				  do (setf at-val nil)
				  else
				  collect val
				  when (and (symbolp val)
					    (eq (find-symbol (string val))
						(find-symbol (string var))))
				  do (setf at-val t)))
	  (apply expr-function values-plist))
       epsilon)))

