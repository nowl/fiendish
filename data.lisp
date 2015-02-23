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
    (:coin3 '(48 0))))

(defstruct debris
  x y
  dx dy)

(defparameter *debris* nil)

(loop with span = 5000 for i below 1000 do
     (push (make-debris :x (- (random span) (/ span 2))
                        :y (- (random span) (/ span 2))
                        :dx (- (random 2.0) 1)
                        :dy (- (random 2.0) 1))
           *debris*))


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
    (:coin3 :coin1)))
