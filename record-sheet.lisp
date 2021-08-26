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

(defclass element ()
  "This defines an `element', which is what gets placed on the board.
If I expand this game up to BF or SBF level, elements will get rolled into units and then formations,
but that is for later. For now, all game pieces are elements.
Eventually, this will need to include images."
  ((name
    "The name of the element. For exampe: Locust lct-1v"
    :initarg :name
    :accessor name)
   (point-value
    "The point value of the element."
    :initarg :pv
    :accessor pv)
   (kind
    "The unit type, must be one of the options from the element-type enum."
    :initarg :kind
    :type element-type
    :accessor kind)
   (size
    "The size. Should be an integer from 1-4."
    :initarg :size
    :accessor size)
   (tmm
    "The Target Movement Modifier. How hard the element is to hit."
    :initarg :tmm
    :accessor tmm)
   (move-distance
    "How many hexes the element can move."
    :initarg :mv-distance
    :accessor mv)
   (move-type
    "How the element moves on the map. Currently, only walking and jumping are
supported."
    :initarg :mv-type
    :type move-type
    :accessor mv-type)
   (role
    "A text description of the role. This will be used to create formation types
for bonuses."
    :initarg :role
    :accessor role)
   (pilot
    "A crew object who represents the pilot of the mech. Eventually, we'll need
to handle multiple crew members (or maybe not?), but we'll deal with that then."
    :initarg :pilot
    :accessor pilot
    :type pilot
    :initform (make-instance 'pilot :name "Shooty McShootface" :skill 4))
   (damages
    "An alist of alists. Must contain the alist `:standard', may optionally
contain other damage types like `SRM' or `IF'."
    :initarg :damages
    :accessor damages)
   (overheat
    "How much extra damage the unit can generate in exchange for gaining heat."
    :initarg :ov
    :accessor overheat)
   (heat
    "How much heat the unit currently has."
    :initarg :heat
    :initform 0
    :accessor heat)
   (max-armor
    "Maximum armor value."
    :initarg :max-armor
    :accessor armor)
   (current-armor
    "Current armor value."
    :initarg :current-armor
    :accessor current-armor)
   (max-structure
    "Maximum structure value."
    :initarg :max-structure
    :accessor struct)
   (current-structure
    "Current structure value."
    :initarg :current-structure
    :accessor current-struct)
   (specials
    "A list of special abilities the unit has."
    :initarg :specials
    :accessor specials)
   (crits
    "A list of critical hits the unit has sustained. Must be members of the
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
