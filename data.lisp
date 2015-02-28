(in-package :fiendish-rl)

(defparameter *screen-width* 640)
(defparameter *screen-height* 360)

(defstruct player-ship
  x
  y
  dir
  vel-x
  vel-y
  accel-x
  accel-y
  thrust
  last-fired-time)

(defparameter *player-ship* (make-player-ship :x 0 :y 0 :dir :ship-up :thrust 1.5 :vel-x 0 :vel-y 0 :accel-x 0 :accel-y 0 :last-fired-time 0))

#|
(defun ship-orientation ()
  (let ((dx (player-ship-vel-x *player-ship*))
        (dy (player-ship-vel-y *player-ship*)))
    (cond
      ((> (abs dx) (abs dy))
       (if (plusp dx) :ship-right :ship-left))
      (t (if (plusp dy) :ship-down :ship-up)))))
|#

(defun texture-position (obj)
  (ecase obj
    (:ship-up '(0 16))
    (:ship-down '(16 16))
    (:ship-right '(32 16))
    (:ship-left '(48 16))
    (:debris '(96 0))
    (:coin1 '(16 0))
    (:coin2 '(32 0))
    (:coin3 '(48 0))
    (:player-fire '(64 0))
    (:enemy-ship1 '(0 0))
    (:enemy-ship2 '(0 32))
    (:star '(16 32))))

(defstruct debris
  x y
  dx dy)

(defparameter *debris* nil)

(defstruct coin
  x y
  state
  to-flip)

(defparameter *coins* nil)

(loop with span = 5000 for i below 1000 do
     (push (make-coin :x (- (random span) (/ span 2))
                      :y (- (random span) (/ span 2))
                      :state (nth (random 3) '(:coin1 :coin2 :coin3))
                      :to-flip (random 30))
           *coins*))

(defun next-coin-state (coin)
  (ecase (coin-state coin)
    (:coin1 :coin2)
    (:coin2 :coin3)
    (:coin3 :coin4)
    (:coin4 :coin1)))

(defun coin-state-to-image (coin)
  (ecase (coin-state coin)
    (:coin1 :coin1)
    (:coin2 :coin2)
    (:coin3 :coin3)
    (:coin4 :coin2)))

(defstruct player-fire
  x y
  dir)

(defparameter *player-fire* nil)
(defparameter *player-fire-speed* 15)
(defparameter *player-fire-repeat-ms* 200)

(defstruct enemy-ship
  x y
  dx dy
  type
  behavior)

(defparameter *enemy-ships* nil)
(defparameter *next-enemy-ship-check* 0)
(defparameter *enemy-ship-check-ms* 250)


(defparameter *next-debris-check* 0)
(defparameter *debris-check-ms* 100)

(defstruct star
  x y
  dist)

(defparameter *stars* nil)
(defparameter *star-extents* 1000)

(defun player-sector ()
  (list (floor (/ (* 0.2 (player-ship-x *player-ship*)) *star-extents*))
        (floor (/ (* 0.2 (player-ship-y *player-ship*)) *star-extents*))))

(loop with span = *star-extents* for i below 100 do
     (push (make-star :x (random span)
                      :y (random span)
                      :dist (random 0.1))
           *stars*))

(defun ship-move-towards-player (enemy)
  (let ((d (ecase (enemy-ship-type enemy)
             (0 2)
             (1 1))))
    (setf (enemy-ship-dx enemy) 
          (if (> (player-ship-x *player-ship*) (enemy-ship-x enemy))
              d (- d))
          (enemy-ship-dy enemy) 
          (if (> (player-ship-y *player-ship*) (enemy-ship-y enemy))
              d (- d)))))

(defun manhattan-dist-to-player (x y)
  (+ (abs (- x (player-ship-x *player-ship*)))
     (abs (- y (player-ship-y *player-ship*)))))
