;;;; 

(asdf:defsystem #:cl-with-c
  :description "WITH-C macro for working with foreign structures"
  :author "stacksmith <fpgasm@apple2.x10.mx>"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "with-c")))
