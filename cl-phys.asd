(asdf:defsystem :cl-phys
  :version 0.1
  :license "GPLv3"
  :author "Julian Wolf <julian.wolf@mail.mcgill.ca>"
  :description "cl-phys attempts to wrap up
  useful fitting and data visualization methods
  into a convenient suite for Common Lisp."
  :depends-on (#:cl-phys.fitting
               ; #:cl-phys.visualization
               #:cl-phys.calculus)
  :components ((:file "package")))
