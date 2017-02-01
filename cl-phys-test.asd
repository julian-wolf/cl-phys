(asdf:defsystem #:cl-phys-test
  :version "0.1"
  :license "GPLv3"
  :author "Julian Wolf <julian.wolf@mail.mcgill.ca>"
  :description "test-suite for cl-phys"
  :depends-on (#:cl-phys
	       #:cl-coveralls
	       #:lisp-unit)
  :components ((:file "cl-phys-test")))
