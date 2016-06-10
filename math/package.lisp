(defpackage #:cl-phys.math
  (:use :cl)
  (:export :differentiate-analytic
           :eval-derivative-analytic
           :eval-derivative-numeric
           :expression->function
           :make-expression
           :valid-expression-types
           :jacobian
           :matrix-transpose
           :matrix-multiply
           :solve-system))
