(in-package :fiendish-rl)

(defparameter *font* nil)

(defun blit (texture pos)
  (let ((tex (texture-position texture)))
    (destructuring-bind (tex-x tex-y) tex
      (destructuring-bind (x y) pos
        (fiendish-rl.ffi:blit tex-x tex-y 16 16 (round x) (round y) 32 32)))))

(defun world-to-screen (x y &key (mod 1))
  (let ((ox (- (/ *screen-width* 2) 16))
        (oy (- (/ *screen-height* 2) 16))
        (dx (player-ship-x *player-ship*))
        (dy (player-ship-y *player-ship*)))
    (list (+ x (* (- ox dx) mod)) (+ y (* (- oy dy) mod)))))

(defun within-screen (x y &rest args)
  "Tests if the world coordinates x,y are within the screen. If so
then return those screen coordinates, otherwise nil"
  (destructuring-bind (sx sy) (apply #'world-to-screen x y args)
    (if (and (>= sx (- 0 32)) (>= sy (- 0 32)) (< sx *screen-width*) (< sy *screen-height*))
        (list sx sy)
        (list nil nil))))

(defun draw ()
  (loop for s in *stars* do
       (destructuring-bind (sx sy) (within-screen (star-x s)
                                                  (star-y s)
                                                  :mod #+deb(star-dist s) 0.2)
         (when sx
           (blit :player-fire (list sx sy)))))

  (loop for d in *debris* do
       (destructuring-bind (sx sy) (within-screen (debris-x d) (debris-y d))
         (when sx
           (blit :debris (list sx sy)))))

  (loop for c in *coins* do
       (destructuring-bind (sx sy) (within-screen (coin-x c) (coin-y c))
         (when sx
           (blit (coin-state-to-image c) (list sx sy)))))
  
  (fiendish-rl.ffi:draw-text *font* "This is a test! Can you read this?" 
                             0 10 0 0 240 255)
  (fiendish-rl.ffi:draw-text *font* "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
                             0 30 0 0 240 255)
  (fiendish-rl.ffi:draw-text *font* "abcdefghijklmnopqrstuvwxyz" 
                             0 50 0 0 240 100)
  (fiendish-rl.ffi:draw-text *font* "1234567890"
                             0 70 0 0 240 255)
  (blit (player-ship-dir *player-ship*) (list (- (/ *screen-width* 2) 16) (- (/ *screen-height* 2) 16)))

  (loop for f in *player-fire* do
       (destructuring-bind (sx sy) (within-screen (player-fire-x f) (player-fire-y f))
         (when sx
           (blit :player-fire (list sx sy))))))
