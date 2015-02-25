(in-package :fiendish-rl)

(defparameter *epsilon* 1e-6)

(defparameter *ticks-per-second* 60)
(defparameter *ms-per-tick* (/ 1000 *ticks-per-second*))
(defparameter *next-tick-ms* 0)
(defparameter *max-frame-skip* 5)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

(defparameter *running* t)

(defparameter *keyboard-state* (make-hash-table))

(defparameter *keyboard-initial-delay-ms* 60)
(defparameter *keyboard-repeat-delay-ms* 60)

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

(defun player-fire ()
  (let ((dir (player-ship-dir *player-ship*)))
    (push (make-player-fire :x (player-ship-x *player-ship*) :y (player-ship-y *player-ship*) :dir dir)
          *player-fire*)))    

(defun update-player-fire ()
  (let (delete-list)
    (loop for fire in *player-fire* do
         (ecase (player-fire-dir fire)
           (:ship-up (incf (player-fire-y fire) (- *player-fire-speed*)))
           (:ship-down (incf (player-fire-y fire) *player-fire-speed*))
           (:ship-left (incf (player-fire-x fire) (- *player-fire-speed*)))
           (:ship-right (incf (player-fire-x fire) *player-fire-speed*)))
         (destructuring-bind (sx sy) (within-screen (player-fire-x fire) (player-fire-y fire))
           (declare (ignore sy))
           (when (not sx)
             (push fire delete-list))))
    (loop for d in delete-list do
         (setf *player-fire* (delete d *player-fire* :test #'eq)))))

(defun get-held-keys ()
  (let (results)
    (loop with tick = (fiendish-rl.ffi:getticks)
       for key being the hash-key of *keyboard-state* using (hash-value key-state) do
         (when (key-state-pressed key-state)
           (cond
             ((and (key-state-in-repeat key-state) 
                   (>= (- tick (key-state-time-pressed key-state)) *keyboard-repeat-delay-ms*))
              (push (list key (key-state-mod key-state)) results)
              (setf (key-state-time-pressed key-state) tick))
             ((and (not (key-state-in-repeat key-state))
                   (>= (- tick (key-state-time-pressed key-state)) *keyboard-initial-delay-ms*))
              (push (list key (key-state-mod key-state)) results)
              (setf (key-state-time-pressed key-state) tick
                    (key-state-in-repeat key-state) t)))))
    results))

(defun keyboard-update (type value mod)
  (let ((kstate (gethash value *keyboard-state*)))
    (ecase type
      (:press (cond
                ((and kstate (key-state-pressed kstate)) (return-from keyboard-update nil))
                (kstate (setf (key-state-pressed kstate) t
                              (key-state-time-pressed kstate) (fiendish-rl.ffi:getticks)
                              (key-state-in-repeat kstate) nil))
                (t (setf (gethash value *keyboard-state*) (make-key-state :mod mod
                                                                          :pressed t
                                                                          :in-repeat nil
                                                                          :time-pressed (fiendish-rl.ffi:getticks)))))
              (handle-keypress value mod))
      (:release (setf (key-state-pressed kstate) nil)))))

(defun update ()
  (loop for key-mod in (get-held-keys) do
       (apply #'handle-keypress key-mod))

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
         (setf (coin-state c) (next-coin-state c))))

  (update-player-fire))
       

(defun handle-keypress (key mod)
  (cond
    ((= key (char-code #\escape)) (setf *running* nil))
    ((= key (char-code #\d)) (move-player :right))
    ((= key (char-code #\a)) (move-player :left))
    ((= key (char-code #\w)) (move-player :up))
    ((= key (char-code #\s)) (move-player :down))
    ((= key (char-code #\space)) (player-fire))))

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
