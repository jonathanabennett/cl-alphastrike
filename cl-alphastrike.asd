;;;; cl-alphastrike.asd

(asdf:defsystem #:cl-alphastrike
  :description "Describe cl-alphastrike here"
  :author "Jonathan A. Bennett"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:ltk)
  :components ((:file "package")
               (:file "cl-alphastrike")))
