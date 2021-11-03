(in-package #:cl-alphastrike)

;; Begin by defining a Hexagon class. We are using the Cubic constructor and
;; storage method described by Amin at Red Blob Games.
(defclass hexagon ()
  ((q
    :initarg :q
    :accessor q
    :documentation "location on the Q axis.")
   (r
    :initarg :r
    :accessor r
    :documentation "location on the R axis.")
   (s
    :initarg :s
    :accessor s
    :documentation "location on the S axis.")))

(defun make-hexagon (&key q r s)
  (if (eq (+ q r s) 0)
      (make-instance 'hexagon
                     :q q
                     :r r
                     :s s)))

(defmethod same-hex ((hex1 hexagon) (hex2 hexagon))
  "Hexagons are equal if their q, r, and s coordinates are the same."
  (and (= (s hex1) (s hex2))
       (= (r hex1) (r hex2))
       (= (q hex1) (q hex2))))

(defmethod hex-addition ((a hexagon) (b hexagon))
  "Uses Cartesian addition to add two hexagons together."
  (make-hexagon :q (+ (q a) (q b))
                :r (+ (r a) (r b))
                :s (+ (s a) (s b))))

(defmethod hex-subtract ((a hexagon) (b hexagon))
  "Uses Cartesian subtraction to add two hexagons together."
  (make-hexagon :q (- (q a) (q b))
                :r (- (r a) (r b))
                :s (- (s a) (s b))))

(defmethod hex-multiply ((a hexagon) x)
  "Uses Cartesian multiplication to add two hexagons together."
  (make-hexagon :q (* (q a) x)
                :r (* (r a) x)
                :s (* (s a) x)))

(defun hex-length (hex)
  "The length of the distance between two hexagons is calculated similarly to
Manhattan distances with a square grid, but you half the sum to get the final
distance. The function cannot be called without hex-distance to produce the
appropriate hexagon first."
  (/ (+ (abs (q hex)) (abs (r hex)) (abs (s hex))) 2))

(defmethod hex-distance ((a hexagon) (b hexagon))
  "This produces the hex we use in hex-length."
  (hex-length (hex-subtract a b)))

;; We use the following Vector to calculate neighbors.
(defvar *hex-directions* (vector (make-hexagon :q 1 :r 0 :s -1)
                                     (make-hexagon :q 1 :r -1 :s 0)
                                     (make-hexagon :q 0 :r -1 :s 1)
                                     (make-hexagon :q -1 :r 0 :s 1)
                                     (make-hexagon :q -1 :r 1 :s 0)
                                     (make-hexagon :q 0 :r 1 :s -1))
  "This vector describes the offsets to calculate neighbors.")

(defun hex-direction (direction)
  (if (and (<= 0 direction) (> 6 direction))
      (elt *hex-directions* direction)))

(defmethod hex-neighbor ((hex hexagon) direction)
  (hex-addition hex (hex-direction direction)))

(defclass Point ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)))

(defun make-point (&key x y)
  (make-instance 'Point :x x :y y))
;; Display Implementation

(defstruct layout
  hex-to-pixel-matrix
  pixel-to-hex-matrix
  start-angle
  x-size
  y-size
  x-origin
  y-origin)

;; This function does the math to convert from a q,r,s address to an x,y
;; position for the center fo the hex.

(defun hex-to-pixel (hex layout)
  (let ((vec (layout-hex-to-pixel-matrix layout)))
    (make-point :x (+ (* (+ (* (elt vec 0) (q hex)) (* (elt vec 1)(r hex))) (layout-x-size layout)) (layout-x-origin layout))
                :y (+ (* (+ (* (elt vec 2)(q hex)) (* (elt vec 3)(r hex))) (layout-y-size layout)) (layout-y-origin layout)))))

(defun pixel-to-hex (mouse-point layout)
  (let* ((vec (layout-pixel-to-hex-matrix layout))
         (modified-point (make-point :x (/ (- (x mouse-point) (layout-x-origin layout)) (layout-x-size layout))
                                     :y (/ (- (x mouse-point) (layout-y-origin layout)) (layout-y-size layout))))
         (calc-q (+ (* (elt vec 0) (x modified-point)) (* (elt vec 1) (y modified-point))))
         (calc-r (+ (* (elt vec 2) (x modified-point)) (* (elt vec 3) (y modified-point)))))
    (make-hexagon :q calc-q :r calc-r :s (+ (* calc-q -1) (* calc-r -1)))))

(defun find-hex-corner (corner layout)
  (let ((angle (* 2.0 pi (/ (+ (layout-start-angle layout) corner) 6))))
    (make-point :x (+ (* (layout-x-size layout) (cos angle)) (layout-x-origin layout))
                :y (+ (* (layout-y-size layout) (sin angle)) (layout-y-origin layout)))))

(defun point-to-list (point)
  (list (x point) (y point)))

(defun draw-hex (hex layout)
  (let ((center (hex-to-pixel hex layout))
        (points '()))
    (dotimes (i 6)
      (push (point-to-list (find-hex-corner i layout)) points))
     points))

(defun pixel-to-hex (origin-point point-size point)
  "Not yet implemented.")
