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
  thrust)

(defparameter *player-ship* (make-player-ship :x 0 :y 0 :dir :ship-up :thrust 1.5 :vel-x 0 :vel-y 0 :accel-x 0 :accel-y 0))

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

(defstruct enemy-ship
  x y
  dx dy
  accx accy
  type
  lifetime
  behavior)

(defparameter *enemy-ships* nil)
(defparameter *next-enemy-ship-check* 0)
(defparameter *enemy-ship-check-ms* 200)


(defparameter *next-debris-check* 0)
(defparameter *debris-check-ms* 75)

(defparameter *next-coin-check* 0)
(defparameter *coin-check-ms* 1500)

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
             (0 0.04)
             (1 0.02))))
    (setf (enemy-ship-accx enemy) 
          (if (> (player-ship-x *player-ship*) (enemy-ship-x enemy))
              d (- d))
          (enemy-ship-accy enemy) 
          (if (> (player-ship-y *player-ship*) (enemy-ship-y enemy))
              d (- d)))))

(defun cutoff-player-x (enemy)
  (let ((d (ecase (enemy-ship-type enemy)
             (0 0.1)
             (1 0.05))))
    (setf (enemy-ship-accx enemy) 
          (if (> (player-ship-x *player-ship*) (enemy-ship-x enemy))
              d (- d)))))

(defun cutoff-player-y (enemy)
  (let ((d (ecase (enemy-ship-type enemy)
             (0 0.1)
             (1 0.05))))
    (setf (enemy-ship-accy enemy) 
          (if (> (player-ship-y *player-ship*) (enemy-ship-y enemy))
              d (- d)))))


(defun manhattan-dist-to-player (x y)
  (+ (abs (- x (player-ship-x *player-ship*)))
     (abs (- y (player-ship-y *player-ship*)))))

(defun aabb-overlap (c1x c1y r1x r1y c2x c2y r2x r2y)
  (cond
    ((> (abs (- c1x c2x)) (+ r1x r2x)) nil)
    ((> (abs (- c1y c2y)) (+ r1y r2y)) nil)
    (t t)))
