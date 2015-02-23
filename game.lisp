(in-package :fiendish-rl)

(defparameter *epsilon* 1e-6)

(defparameter *ticks-per-second* 60)
(defparameter *ms-per-tick* (/ 1000 *ticks-per-second*))
(defparameter *next-tick-ms* 0)
(defparameter *max-frame-skip* 5)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

(defparameter *font* nil)

(defparameter *running* t)

(defparameter *keyboard-state* (make-hash-table))

(defstruct key-state
  mod
  pressed
  in-repeat
  time-pressed)

(defun move-player (dir)
  (ecase dir
    (:up (setf (player-ship-accel-y *player-ship*) (- (player-ship-thrust *player-ship*))
               (player-ship-dir *player-ship*) :ship-up))
    (:down (setf (player-ship-accel-y *player-ship*) (player-ship-thrust *player-ship*)
                 (player-ship-dir *player-ship*) :ship-down))
    (:left (setf (player-ship-accel-x *player-ship*) (- (player-ship-thrust *player-ship*))
                 (player-ship-dir *player-ship*) :ship-left))
    (:right (setf (player-ship-accel-x *player-ship*) (player-ship-thrust *player-ship*)
                  (player-ship-dir *player-ship*) :ship-right))))

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
    (setf (player-ship-vel-y *player-ship*) 0))

  (loop for d in *debris* do
       (incf (debris-x d) (debris-dx d))
       (incf (debris-y d) (debris-dy d)))
  
  (loop for c in *coins* do
       (decf (coin-to-flip c))
       (when (<= (coin-to-flip c) 0)
         (setf (coin-to-flip c) 30)
         (setf (coin-state c) (next-coin-state c)))))
       

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
  (unwind-protect (progn (fiendish-rl.ffi:init "fiendish" 1280 720 *screen-width* *screen-height*)
                         (gameloop))
    (fiendish-rl.ffi:destroy)))
