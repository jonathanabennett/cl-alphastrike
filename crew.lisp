(in-package :cl-alphastrike)

(defclass pilot ()
  "A pilot of a death machine."
  ((name
  "Pilot's name."
    :initarg :name
    :accessor name)
   (skill
    "Pilot's skill level. 4 is the default."
    :initarg :skill
    :initform 4
    :accessor skill)))

(defgeneric display (obj)
  (:documentation "Pretty print an object for display in the GUI"))


(defmethod display ((obj pilot))
  "Formats the pilot for display in the record sheet."
  (format nil "~A: ~A" (name obj) (skill obj)))
