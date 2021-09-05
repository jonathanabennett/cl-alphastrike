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
  (let* ((rs-label          (make-instance 'label :master rs-frame :text rs-label-text :font "Helvetica 24" :anchor :n))
         (general-info      (make-instance 'frame :master rs-frame :borderwidth 1 :relief :raised))
         (heat-info         (make-instance 'frame :master rs-frame :borderwidth 1 :relief :raised))
         (damage-info       (make-instance 'frame :master rs-frame :borderwidth 1 :relief :raised))
         (armor-bubbles     (make-instance 'labelframe :master damage-info :text "armor"))
         (structure-bubbles (make-instance 'labelframe :master damage-info :text "structure"))
         (unit-name-label   (make-instance 'label :text "Unit Name:" :font "Helvetica 20 bold" :master rs-frame :anchor :w))
         (name-label        (make-instance 'label :text (name element) :font "Helvetica 20" :master rs-frame :anchor :w))
         (pv-label          (make-instance 'label :text (format nil "PV: ~A" (pv element)) :master rs-frame :anchor :w))
         (type-label        (make-instance 'label :text (format nil "Type: ~A" (kind element)) :master general-info :anchor :w))
         (size-label        (make-instance 'label :text (format nil "Size: ~A" (size element)) :master general-info :anchor :w))
         (tmm-label         (make-instance 'label :text (format nil "TMM: ~A" (tmm element)) :master general-info :anchor :w))
         (move-label        (make-instance 'label :text (format nil "MV: ~A:~A" (mv element) (mv-type element)) :master general-info :anchor :w))
         (role-label        (make-instance 'label :text (format nil "Role: ~A" (role element)) :master general-info :anchor :w))
         (pilot-label       (make-instance 'label :text (format nil "Pilot: ~A" (display (pilot element))) :master general-info :anchor :w))
         (damage-widget     (damages-dropdown (damages element) rs-frame))
         (ov-label          (make-instance 'label :text (format nil "OV: ~A" (overheat element)) :master heat-info :anchor :w))
         (heat-label        (make-instance 'label :text (format nil "Heat: ~A" (heat element)) :master heat-info :anchor :w))
         (crits-label       (make-instance 'label :text (format nil "Critical Hits: ~A" (crits element)) :master damage-info :anchor :w))
         (specials-label    (make-instance 'label :text (format nil "Specials: ~A" (specials element)) :master rs-frame :anchor :w)))
    (grid rs-label 0 0 :columnspan 6 :sticky :we)
    (grid unit-name-label 1 0)
    (grid name-label 1 1 :columnspan 4)
    (grid pv-label 1 6 :padx 5)
    (grid general-info 2 0 :columnspan 6 :pady 2 :sticky :we)
    (grid heat-info 4 0 :columnspan 6 :pady 2 :sticky :we)
    (grid damage-info 5 0 :columnspan 6 :pady 2 :sticky :we)
    (grid specials-label 6 0 :columnspan 6 :sticky :we)
    (make-damage-bubbles (current-armor element) (armor element) armor-bubbles)
    (grid armor-bubbles 0 0)
    (make-damage-bubbles (current-struct element) (struct element) structure-bubbles)
    (grid structure-bubbles 1 0)
    (grid type-label 0 0)
    (grid size-label 0 1)
    (grid tmm-label 0 2)
    (grid move-label 0 3)
    (grid role-label 1 0)
    (grid pilot-label 1 1 :columnspan 3)
    (grid (make-instance 'label :text "Attacks" :master rs-frame) 3 0)
    (setf (text damage-widget) (display (car (damages element))))
    (grid damage-widget 3 1 :columnspan 5 :pady 2 :sticky :we)
    (grid ov-label 0 0)
    (grid heat-label 0 1)
    (grid crits-label 0 1 :rowspan 2)))

(defun load-data ()
  "Load the contents of the data directory in prepration for execution."
  (load "data/units/locust-lct-1v.lisp")
  (load "data/units/phoenix-hawk-pxh-1d.lisp")
  (load "data/units/marauder-mad-3r.lisp")
  (load "data/units/longbow-lgb-0w.lisp")
  )

(defun create-main-window ()
  (let* ((main-window (make-instance 'frame :name "cl-alphastrike"))
         (unit-list ()))
    (wm-title *tk* "Alphastrike")
    (configure main-window :padding "3 3 12 12")
    (grid main-window 0 0)
    (let ((map (make-instance 'canvas :master main-window :height 800 :width 800)))
      ;; Call fill map function here
      (grid map 0 0 :rowspan 3)
      (create-text map 10 10 "Map Goes Here"))
    (let ((my-sheet (make-instance 'frame :master main-window :borderwidth 2 :relief :ridge))
          (mek (phoenix-hawk-pxh-1d)))
      (configure my-sheet :padding "3 0 0 12")
      (grid my-sheet 0 1)
      (display-record-sheet mek my-sheet "Active Unit"))
    (let ((target-sheet (make-instance 'frame :master main-window :borderwidth 2 :relief :ridge))
          (mek (locust-lct-1v)))
      (grid target-sheet 1 1)
      (setf (current-armor mek) 0)
      (setf (current-struct mek) 1)
      (display-record-sheet mek target-sheet "Targeted Unit"))))
(defun main ()
  "Main loop. Currently just display the record sheet."
  (load-data)
  (with-ltk ()
    (create-main-window)))
