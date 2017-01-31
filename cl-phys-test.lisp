(ql:quickload "lisp-unit")
#-xlisp-test (load "cl-phys")

(in-package #:cl-user)

(defpackage #:cl-phys-test
  (:use #:cl #:cl-coveralls #:prove))

(in-package #:cl-phys-test)

;; TODO: tests
