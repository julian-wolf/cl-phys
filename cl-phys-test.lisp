(in-package #:cl-user)

(defpackage #:cl-phys-test
  (:use #:cl
	#:cl-coveralls
	#:lisp-unit
	#:cl-phys))

(in-package #:cl-phys-test)

;; TODO: tests

(define-test auto-pass
    (assert-equal 5 5))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :cl-phys-test))
