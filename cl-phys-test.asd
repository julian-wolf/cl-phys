(asdf:defsystem #:cl-phys-test
  :version "0.1"
  :license "GPLv3"
  :author "Julian Wolf <julian.wolf@mail.mcgill.ca>"
  :description "test-suite for cl-phys"
  :depends-on (#:cl-phys.fitting)
  :components ((:file "cl-phys-test")))
