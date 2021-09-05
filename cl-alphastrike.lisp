;;;; cl-alphastrike.lisp

(in-package #:cl-alphastrike)

(defun damages-dropdown (damages-list rs-frame)
  (make-instance 'combobox :values (mapcar 'display damages-list) :master rs-frame))

(defun make-damage-bubbles (current max parent-frame)
  "Rewrite this so that the label is a label text label and the canvas is perfectly sized to hold the appropriate number of bubbles.
Then, use the grid manager to pack them both into the appropriate parent frame."
  (let ((bubbles (make-instance 'canvas :master parent-frame :width (+ (* max 15) 5) :height 15)))
    (progn (dotimes (i max)
             (let ((circle (create-oval bubbles (+ (* i 15) 5) 5 (+ (* i 15) 15) 15)))
               (if (> (+ i 1) current)
                   (itemconfigure bubbles circle :fill :black)))))
    (pack bubbles)))

(defun display-record-sheet (element rs-frame rs-label-text)
  "This takes an `element' object as defined in record-sheet.lisp and formats it for display using ltk."
  (let ((unit-name-label (make-instance 'label :text "Unit Name:" :font "Helvetica 20 bold" :master rs-frame :anchor :w))
        (name            (make-instance 'label :text (name element) :font "Helvetica 20" :master rs-frame :anchor :sw))
        (pv              (make-instance 'label :text (format nil "PV: ~A" (pv element)) :master rs-frame :anchor :w))
        (general-info    (make-instance 'labelframe :master rs-frame :text "General"))
        (attack-info     (make-instance 'labelframe :master rs-frame :text "Attacks"))
        (heat-info       (make-instance 'labelframe :master rs-frame :text "Heat"))
        (damage-info     (make-instance 'labelframe :master rs-frame :text "Damage"))
        (specials        (make-instance 'label :text (format nil "Specials: ~A" (specials element)) :master rs-frame :anchor :w)))
  (grid unit-name-label 0 0)
  (grid name 0 1 :columnspan 3)
  (grid pv 0 5 :columnspan 2)
  (grid general-info 1 0 :columnspan 6)
  (grid attack-info 2 0 :columnspan 6)
  (grid heat-info 3 0 :columnspan 6)
  (grid damage-info 4 0 :columnspan 6)
  (grid specials 5 0 :columnspan 6)
  (grid (make-instance 'label :text (format nil "Type: ~A" (kind element)) :master general-info :anchor :w) 0 0)
  (grid (make-instance 'label :text (format nil "Size: ~A" (size element)) :master general-info :anchor :w) 0 1)
  (grid (make-instance 'label :text (format nil "TMM: ~A" (tmm element)) :master general-info :anchor :w) 0 2)
  (grid (make-instance 'label :text (format nil "MV: ~A:~A" (mv element) (mv-type element)) :master general-info :anchor :w) 0 3)
  (grid (make-instance 'label :text (format nil "Role: ~A" (role element)) :master general-info :anchor :w) 1 0)
  (grid (make-instance 'label :text (display (pilot element)) :master general-info :anchor :w) 1 1)
  (grid (damages-dropdown (damages element) attack-info) 0 0)
  (grid (make-instance 'label :text (format nil "OV: ~A" (overheat element)) :master heat-info :anchor :w) 0 0)
  (grid (make-instance 'label :text (format nil "Heat: ~A" (heat element)) :master heat-info :anchor :w) 0 1)
  (grid (make-instance 'label :text (format nil "Armor: ~A/~A" (current-armor element) (armor element)) :master damage-info :anchor :w) 0 0)
  (grid (make-instance 'label :text (format nil "Structure: ~A/~A" (current-struct element) (struct element)) :master damage-info :anchor :w) 1 0)
  (grid (make-instance 'label :text (format nil "Critical Hits: ~A" (crits element)) :master damage-info :anchor :w) 0 1 :rowspan 2)))

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (load "data/units/locust-lct-1v.lisp"))

(defun main ()
  "Main loop. Currently just display the record sheet."
  (load-data)
  (with-ltk ()
    (let ((content (make-instance 'frame)))
      (configure content :padding "3 3 12 12")
      (grid content 0 0)
      (display-record-sheet (locust-lct-1v) content))))
