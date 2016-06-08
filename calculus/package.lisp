(defpackage #:cl-phys.calculus
  (:use :cl)
  (:export :differentiate-analytic
           :eval-derivative-analytic
           :eval-derivative-numeric
           :expression->function
           :make-arithmetic-expression
           :valid-expression-types))
