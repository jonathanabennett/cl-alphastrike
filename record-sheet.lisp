(in-package :cl-alphastrike)

(deftype unit-type () '(member :BM :DS))
(deftype move-type () '(member :WALK :JUMP))
(deftype crit () '(member :ENGINE :FIRE-CONTROL :MP :WEAPONS))

(defclass element ()
  ((name
    :initarg :name
    :accessor name)
   (point-value
    :initarg :pv
    :accessor pv)
   (kind
    :initarg :kind
    :type unit-type
    :accessor kind)
   (size
    :initarg :size
    :accessor size)
   (tmm
    :initarg :tmm
    :accessor tmm)
   (move-distance
    :initarg :mv-distance
    :accessor mv)
   (move-type
    :initarg :mv-type
    :type move-type
    :accessor mv-type)
   (role
    :initarg :role
    :accessor role)
   (skill
    :initarg :skill
    :accessor skill
    :initform 4)
   (damage
    :initarg :damage
    :accessor damage)
   (overheat
    :initarg :ov
    :accessor overheat)
   (heat
    :initarg :heat
    :initform 0
    :accessor heat)
   (max-armor
    :initarg :max-armor
    :accessor armor)
   (current-armor
    :initarg :current-armor
    :accessor current-armor)
   (max-structure
    :initarg :max-structure
    :accessor struct)
   (current-structure
    :initarg :current-structure
    :accessor current-struct)
   (specials
    :initarg :specials
    :accessor specials)
   (crits
    :initarg :crits
    :initform '()
    :accessor crits)))

(defun make-element (&optional (current-armor nil) (curent-structure nil) &key name pv kind size tmm mv-distance mv-type role damage ov max-armor max-structure specials crits)
  ((if (eq current-armor nil)
       (setf current-armor max-armor))
   (if (eq current-structure nil)
       (setf current-structure max-armor))
   (make-instance 'element
                 :name name
                 :pv pv
                 :kind kind
                 :size size
                 :tmm tmm
                 :mv-distance mv-type
                 :mv-type mv-type
                 :role role
                 :damage damage
                 :ov ov
                 :current-armor current-armor
                 :max-armor max-armor
                 :current-structure current-structure
                 :max-structure max-structure
                 :specials specials
                 :crits crits)))
