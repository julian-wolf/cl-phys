(defpackage #:cl-phys.math
  (:use :cl)
  (:export :differentiate-analytic
           :eval-derivative-analytic
           :eval-derivative-numeric
           :convert-expression
           :expression->function
           :make-expression
           :valid-expression-types
           :is-linear-in-parameters
           :jacobian
           :matrix-transpose
           :matrix-multiply
           :matrix-inverse
           :identity-matrix
           :diagonal-matrix
           :solve-system))
