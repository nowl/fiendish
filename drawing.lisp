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
       (destructuring-bind (sect-x sect-y) (player-sector)
         (loop for combos in '((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1)) do
              (let ((sx (+ (* *star-extents* (+ (first combos) sect-x)) (star-x s)))
                    (sy (+ (* *star-extents* (+ (second combos) sect-y)) (star-y s))))
                (destructuring-bind (sx sy) (within-screen sx
                                                           sy
                                                           :mod #+deb(star-dist s) 0.2)
                  (when sx
                    (blit :star (list sx sy))))))))

  (loop for d in *debris* do
       (destructuring-bind (sx sy) (within-screen (debris-x d) (debris-y d))
         (when sx
           (blit :debris (list sx sy)))))

  (loop for c in *coins* do
       (destructuring-bind (sx sy) (within-screen (coin-x c) (coin-y c))
         (when sx
           (blit (coin-state-to-image c) (list sx sy)))))
  
  (when (< (fiendish-rl.ffi:getticks) 15000)
    (fiendish-rl.ffi:draw-text *font* "Welcome to Space Runner." 
                               0 10 0 50 240 255)
    (fiendish-rl.ffi:draw-text *font* "W,A,S,D to move around. You'll figure out the rest." 
                               0 30 0 50 240 255)
    (fiendish-rl.ffi:draw-text *font* "Hint: dodge well. Good luck!" 
                               0 50 0 50 240 100))
  (blit (player-ship-dir *player-ship*) (list (- (/ *screen-width* 2) 16) (- (/ *screen-height* 2) 16)))

;;   (fiendish-rl.ffi:draw-text *font* (format nil "Sector: ~a" (player-sector))
;;                              0 10 0 200 0 255)

  (loop for s in *enemy-ships* do
       (destructuring-bind (sx sy) (within-screen (enemy-ship-x s) (enemy-ship-y s))
         (when sx
           (blit (ecase (enemy-ship-type s)
                   (0 :enemy-ship1)
                   (1 :enemy-ship2))
                 (list sx sy)))))

  (loop for f in *player-fire* do
       (destructuring-bind (sx sy) (within-screen (player-fire-x f) (player-fire-y f))
         (when sx
           (blit :player-fire (list sx sy)))))
    
  (fiendish-rl.ffi:draw-text *font* (format nil "Score: ~d" *score*)
                             250 10 (if (= *score* 0) 255 20) 50 (if (= *score* 0) 0 240) 0)
  (fiendish-rl.ffi:draw-text *font* (format nil "Max Score: ~d" *max-score*)
                             400 10 190 150 60 0))
