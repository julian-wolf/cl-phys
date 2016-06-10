(asdf:defsystem #:cl-phys.fitting
  :description "Various methods involving fitting."
  :components ((:file "package")
               (:file "data")
               (:file "least-squares"))
  :serial t)
