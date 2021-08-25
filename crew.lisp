(in-package :cl-alphastrike)

(defclass pilot ()
  ((name
    :initarg :name
    :accessor name)
   (skill
    :initarg :skill
    :initform 4
    :accessor skill)))

(defgeneric display (obj)
  (:documentation "Pretty print an object for display in the GUI"))


(defmethod display ((obj pilot))
  (format nil "~A: ~A" (name obj) (skill obj)))
