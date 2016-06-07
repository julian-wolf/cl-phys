(defpackage #:cl-phys.calculus
  (:use :cl)
  (:export :differentiate-analytic
	   :eval-derivative-analytic
	   :eval-derivative-numeric
	   :expression->function
	   :make-arithmetic-expression
	   :make-sum
	   :make-product
	   :make-difference
	   :make-quotient
	   :make-sin
	   :make-cos
	   :make-tan
	   :make-exp
	   :make-log
	   :make-pow))
