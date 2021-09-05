(in-package :cl-alphastrike)

(deftype elements-type ()
  "This functions like an ENUM, defining the valid list of element types in the game."
  '(member :BM :DS))
(deftype move-type ()
  "This functions like an ENUM, defining the valid ways an element can move in the game."
  '(member :WALK :JUMP))
(deftype crit ()
  "This functions like an ENUM, defining the possible critical hits an element can take in the game."
  '(member :ENGINE :FIRE-CONTROL :MP :WEAPONS))

(defclass damage-value ()
  ((attack-type
    :documentation "What kind of attack is this?"
    :initarg :atk-type
    :accessor atk-type
    :initform :standard)
   (range-brackets
   :documentation "A list containing up to 3 damage values, for S, M, and L range attacks."
   :initarg :range-brackets
   :accessor range-brackets
   :initform '(0 0 0))))

(defun make-damage-value (&key damage-type range-brackets)
  (make-instance 'damage-value :atk-type damage-type :range-brackets range-brackets))

(defmethod display ((obj damage-value))
  "Formats the pilot for display in the record sheet."
  (format nil "~A: ~A" (atk-type obj) (range-brackets obj)))

(defclass element ()
  ((name
    :documentation "The name of the element. For exampe: Locust lct-1v"
    :initarg :name
    :accessor name)
   (point-value
    :documentation "The point value of the element."
    :initarg :pv
    :accessor pv)
   (kind
    :documentation "The unit type, must be one of the options from the element-type enum."
    :initarg :kind
    :type element-type
    :accessor kind)
   (size
    :documentation "The size. Should be an integer from 1-4."
    :initarg :size
    :accessor size)
   (tmm
    :documentation "The Target Movement Modifier. How hard the element is to hit."
    :initarg :tmm
    :accessor tmm)
   (move-distance
    :documentation "How many hexes the element can move."
    :initarg :mv-distance
    :accessor mv)
   (move-type
    :documentation "How the element moves on the map. Currently, only walking and jumping are
supported."
    :initarg :mv-type
    :type move-type
    :accessor mv-type)
   (role
    :documentation "A text description of the role. This will be used to create formation types
for bonuses."
    :initarg :role
    :accessor role)
   (pilot
    :documentation "A crew object who represents the pilot of the mech. Eventually, we'll need
to handle multiple crew members (or maybe not?), but we'll deal with that then."
    :initarg :pilot
    :accessor pilot
    :type pilot
    :initform (make-instance 'pilot :name "Shooty McShootface" :skill 4))
   (damages
    :documentation "An alist of alists. Must contain the alist `:standard', may optionally
contain other damage types like `SRM' or `IF'."
    :initarg :damages
    :accessor damages)
   (overheat
    :documentation "How much extra damage the unit can generate in exchange for gaining heat."
    :initarg :ov
    :accessor overheat)
   (heat
    :documentation "How much heat the unit currently has."
    :initarg :heat
    :initform 0
    :accessor heat)
   (max-armor
    :documentation "Maximum armor value."
    :initarg :max-armor
    :accessor armor)
   (current-armor
    :documentation "Current armor value."
    :initarg :current-armor
    :accessor current-armor)
   (max-structure
    :documentation "Maximum structure value."
    :initarg :max-structure
    :accessor struct)
   (current-structure
    :documentation "Current structure value."
    :initarg :current-structure
    :accessor current-struct)
   (specials
    :documentation "A list of special abilities the unit has."
    :initarg :specials
    :accessor specials)
   (crits
    :documentation "A list of critical hits the unit has sustained. Must be members of the
`crit' type."
    :initarg :crits
    :initform '()
    :accessor crits)))

(defun make-element (&key name pv kind size tmm mv-distance mv-type role damages ov current-armor max-armor current-structure max-structure specials crits)
  "Make an element, checking for values typically missing such as
current-armor and setting them to reasonable defaults."
  (if (eq current-armor nil)
       (setf current-armor max-armor))
   (if (eq current-structure nil)
       (setf current-structure max-structure))
   (make-instance 'element
                 :name name
                 :pv pv
                 :kind kind
                 :size size
                 :tmm tmm
                 :mv-distance mv-distance
                 :mv-type mv-type
                 :role role
                 :damages damages
                 :ov ov
                 :current-armor current-armor
                 :max-armor max-armor
                 :current-structure current-structure
                 :max-structure max-structure
                 :specials specials
                 :crits crits))
