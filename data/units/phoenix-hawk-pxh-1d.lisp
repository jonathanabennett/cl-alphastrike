(in-package :cl-alphastrike)

(defun phoenix-hawk-pxh-1d ()
  "Overview: The Locust is undoubtedly one of the most popular and prevalent
light BattleMechs ever made. First produced in 2499, the almost dozen distinct
factories manufacturing the design quickly spread the design to every power in
human space. Its combination of tough armor (for its size), exceptional speed,
and most importantly, low cost have all contributed to the Locust's success. It
remains the benchmark for many scouting designs, and its continual upgrades have
ensured that it remains just as effective with every new conflict that appears.

Capabilities: As the Locust was first developed as a recon platform, speed is
paramount to the design's philosophy. While many variants change the weaponry to
fill specific tasks or purposes, Locusts are nearly always pressed into service
in ways where they can best take advantage of their speed. When in line
regiments, they can act as a deadly flankers or harassers, and are often used in
reactionary roles to quickly plug holes in a fluid battle line. The structural
form of Locusts themselves are their greatest weakness; with no hands, they are
disadvantaged in phyisical combat and occasionally have difficulty righting
themselves after a fall.

Deployment: One of the most common designs even produced, even the smallest
mercenary or pirate outfits will often field one or more of the design.
Production for the Locust has continued uninterrupted for centuries, and it
plays an important role in the militaries of many smaller nations. The base
LCT-1V was once estimated to account for more than 75% of all Locusts in
existence at the end of the Succession Wars, though these numbers have dropped
with the reappearance of more advanced technology. Still, it remains common in
every military worth note.

systemmanufacturer:CHASSIS:Bergan
systemmode:CHASSIS:VII
systemmanufacturer:ENGINE:LTV
systemmode:ENGINE:160
systemmanufacturer:ARMOR:StarSlab
systemmode:ARMOR:/1
systemmanufacturer:COMMUNICATIONS:Garrett
systemmode:COMMUNICATIONS:T10-B
systemmanufacturer:TARGETING:O/P
systemmode:TARGETING:911 "
  (make-element
   :name "Phoenix Hawk pxh-1d"
   :pv 26
   :kind :bm
   :size 2
   :tmm 2
   :mv-distance 6
   :mv-type :jump
   :role "Skirmisher"
   :damages (list
             (make-damage-value :damage-type :standard
                                :range-brackets '(2 2 0))
             ) ;; To add a new damage type, copy the above two lines and paste them above this parentheses.
   :ov 0
   :max-armor 4
   :max-structure 4
   :specials '(:ENE)
   :crits '()))
