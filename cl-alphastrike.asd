;;;; cl-alphastrike.asd

(asdf:defsystem #:cl-alphastrike
  :description "A port of the Alphastrike board game to Common Lisp."
  :author "Jonathan A. Bennett"
  :license  "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:mcclim)
  :components ((:file "package")
               (:file "crew")
               (:file "map")
               (:file "record-sheet")
               (:file "cl-alphastrike"))
  :build-operation :program-op
  :build-pathname "cl-alphastrike"
  :entry-point "cl-alphastrike:main")
