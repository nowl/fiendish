(in-package :fiendish-rl)

(defun blit (texture pos)
  (let ((tex (texture-position texture)))
    (destructuring-bind (tex-x tex-y) tex
      (destructuring-bind (x y) pos
        (fiendish-rl.ffi:blit tex-x tex-y 16 16 (round x) (round y) 32 32)))))

(defun world-to-screen (x y)
  (let ((ox (- (/ *screen-width* 2) 16))
        (oy (- (/ *screen-height* 2) 16))
        (dx (player-ship-x *player-ship*))
        (dy (player-ship-y *player-ship*)))
    (list (- (+ x ox) dx) (- (+ y oy) dy))))

(defun within-screen (x y)
  "Tests if the world coordinates x,y are within the screen. If so
then return those screen coordinates, otherwise nil"
  (destructuring-bind (sx sy) (world-to-screen x y)
    (if (and (>= sx (- 0 32)) (>= sy (- 0 32)) (< sx *screen-width*) (< sy *screen-height*))
        (list sx sy)
        (list nil nil))))

(defun draw ()
  (loop for d in *debris* do
       (destructuring-bind (sx sy) (within-screen (debris-x d) (debris-y d))
         (when sx
           (blit :debris (list sx sy)))))

  (loop for c in *coins* do
       (destructuring-bind (sx sy) (within-screen (coin-x c) (coin-y c))
         (when sx
           (blit (coin-state c) (list sx sy)))))
  
  (fiendish-rl.ffi:draw-text *font* "This is a test! Can you read this?" 
                             0 10 0 0 240 255)
  (fiendish-rl.ffi:draw-text *font* "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
                             0 30 0 0 240 255)
  (fiendish-rl.ffi:draw-text *font* "abcdefghijklmnopqrstuvwxyz" 
                             0 50 0 0 240 100)
  (fiendish-rl.ffi:draw-text *font* "1234567890"
                             0 70 0 0 240 255)
  (blit (player-ship-dir *player-ship*) (list (- (/ *screen-width* 2) 16) (- (/ *screen-height* 2) 16))))
