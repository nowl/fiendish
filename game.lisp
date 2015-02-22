(in-package :fiendish-rl)

(defparameter *epsilon* 1e-6)

(defparameter *ticks-per-second* 60)
(defparameter *ms-per-tick* (/ 1000 *ticks-per-second*))
(defparameter *next-tick-ms* 0)
(defparameter *max-frame-skip* 5)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

(defparameter *text-draw-x* 0.0)

(defparameter *font* nil)

(defparameter *cycle-x* 0)
(defparameter *running* t)

(defparameter *keyboard-state* (make-hash-table))

(defstruct key-state
  mod
  pressed
  in-repeat
  time-pressed)

(defstruct player-ship
  x
  y
  dir
  vel-x
  vel-y
  accel-x
  accel-y
  thrust)

(defparameter *player-ship* (make-player-ship :x 100 :y 100 :dir :up :thrust 1.5 :vel-x 0 :vel-y 0 :accel-x 0 :accel-y 0))

(defun move-player (dir)
  (ecase dir
    (:up (setf (player-ship-accel-y *player-ship*) (- (player-ship-thrust *player-ship*))))
    (:down (setf (player-ship-accel-y *player-ship*) (player-ship-thrust *player-ship*)))
    (:left (setf (player-ship-accel-x *player-ship*) (- (player-ship-thrust *player-ship*))))
    (:right (setf (player-ship-accel-x *player-ship*) (player-ship-thrust *player-ship*)))))

(defun get-held-keys () )

(defun keyboard-update (type value mod)
  (let ((kstate (gethash value *keyboard-state*)))
    (ecase type
      (:press (if kstate
                  (setf (key-state-pressed kstate) t)
                  (setf (gethash value *keyboard-state*) (make-key-state :mod mod
                                                                         :pressed t
                                                                         :in-repeat nil
                                                                         :time-pressed nil)))
              (handle-keypress value mod))
      (:release (setf (key-state-pressed kstate) nil)))))

(defun draw ()
  (let* ((rad 5)
         (dx (round (* rad (cos *cycle-x*))))
         (dy (round (* rad (sin *cycle-x*)))))
    #+debug(loop for x from 0 to 20 do
         (loop for y from 0 to 15 do
              (fiendish-rl.ffi:blit 0 32 16 16 (+ dx (* x 32)) (+ dy (* y 32)) 32 32))))
  (incf *cycle-x* 0.1)
  (let ((x (round *text-draw-x*)))
    (fiendish-rl.ffi:draw-text *font* "This is a test! Can you read this?" 
                                0 10 0 0 240 255)
    (fiendish-rl.ffi:draw-text *font* "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
                                0 30 0 0 240 255)
    (fiendish-rl.ffi:draw-text *font* "abcdefghijklmnopqrstuvwxyz" 
                                0 50 0 0 240 100)
    (fiendish-rl.ffi:draw-text *font* "1234567890"
                                0 70 0 0 240 255)
    (incf *text-draw-x* 0.5)
    (if (> *text-draw-x* (- 200 10))
        (setf *text-draw-x* 0)))
  (fiendish-rl.ffi:blit 0 16 16 16 (round (player-ship-x *player-ship*)) (round (player-ship-y *player-ship*)) 32 32))

(defun update ()
  (incf (player-ship-vel-x *player-ship*) (player-ship-accel-x *player-ship*))
  (incf (player-ship-vel-y *player-ship*) (player-ship-accel-y *player-ship*))

  (incf (player-ship-x *player-ship*) (player-ship-vel-x *player-ship*))
  (incf (player-ship-y *player-ship*) (player-ship-vel-y *player-ship*))

  (setf (player-ship-accel-x *player-ship*) 0
        (player-ship-accel-y *player-ship*) 0
        (player-ship-vel-x *player-ship*) (* 0.95 (player-ship-vel-x *player-ship*))
        (player-ship-vel-y *player-ship*) (* 0.95 (player-ship-vel-y *player-ship*)))

  (when (< (abs (player-ship-vel-x *player-ship*)) *epsilon*)
    (setf (player-ship-vel-x *player-ship*) 0))

  (when (< (abs (player-ship-vel-y *player-ship*)) *epsilon*)
    (setf (player-ship-vel-y *player-ship*) 0)))


(defun handle-keypress (key mod)
  (cond
    ((= key (char-code #\escape)) (setf *running* nil))
    ((= key (char-code #\d)) (move-player :right))
    ((= key (char-code #\a)) (move-player :left))
    ((= key (char-code #\w)) (move-player :up))
    ((= key (char-code #\s)) (move-player :down))))  

(defun gameloop ()
  (setf *startup-time-ms* (fiendish-rl.ffi:getticks)
        *draw-count* 0
        *font* (fiendish-rl.ffi:open-font "DroidSansMono.ttf" 12)
        *next-tick-ms* 0
        *running* t)
  (clrhash *keyboard-state*)

  (fiendish-rl.ffi:set-texture-source "sprites.png")
  
  (loop while *running* do
       (let ((tick (fiendish-rl.ffi:getticks))
             (frame-skip 0))

         (loop while t do
              (destructuring-bind (result key mod) (fiendish-rl.ffi:pollevent)
                (if result
                    (keyboard-update result key mod)
                    (return))))                  
           
         (loop while (and (>= tick *next-tick-ms*) (< frame-skip *max-frame-skip*)) do
              (update)
              (incf *next-tick-ms* *ms-per-tick*)
              (incf frame-skip))
         
         (fiendish-rl.ffi:clear)
         (draw)
         (fiendish-rl.ffi:draw)
         (incf *draw-count*)))

  (let* ((end-time-ms (fiendish-rl.ffi:getticks))
         (elapsed-time (/ (- end-time-ms *startup-time-ms*) 1000.0))
         (average-fps (float (/ *draw-count* elapsed-time))))
    (format t "frames drawn = ~a~%" *draw-count*)
    (format t "gameloop elapsed time = ~a~%" elapsed-time)
    (format t "average fps = ~a~%" average-fps)))

(defun run ()
  (unwind-protect (progn (fiendish-rl.ffi:init "fiendish" 1280 720 640 360)
                         (gameloop))
    (fiendish-rl.ffi:destroy)))
