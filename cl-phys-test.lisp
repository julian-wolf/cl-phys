(in-package #:cl-user)

(defpackage #:cl-phys-test
  (:use #:cl
	#:cl-coveralls
	#:lisp-unit
	#:cl-phys)
  (:export :run-all-tests))

(in-package #:cl-phys-test)

(defparameter *print-errors* t)
(defparameter *print-failures* t)

;; TODO: tests

(define-test auto-pass
    (assert-equal 5 5))

(defun run-all-tests ()
  "Runs all tests, returning 0 if they pass and -1 otherwise"
  (let ((db (run-tests :all :cl-phys-test)))
    (if (and (null (failed-tests db))
	     (null (error-tests db)))
	0
	-1)))
