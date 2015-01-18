(in-package :fiendish-rl)

(defparameter *width* 1920)
(defparameter *height* (floor (/ (* *width* 9) 16)))

(defparameter *src-rect* (sdl2:make-rect 0 0 9 16))
(defparameter *dst-rect* (sdl2:make-rect 0 0 9 16))
(defparameter *renderer* nil)
(defparameter *texture* nil)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

;;(defparameter *screen-bg* (make-array '(120 38) :element-type 'fixnum))
(defparameter *screen-fg* (make-array '(120 38) :element-type 'fixnum))

(defun putchar (char-or-code x y)
  (setf (aref *screen-fg* x y)
        (etypecase char-or-code
          (fixnum char-or-code)
          (character (char-code char-or-code)))))

(putchar #\W 0 0)
(putchar #\e 1 0)
(putchar #\l 2 0)
(putchar #\c 3 0)
(putchar #\o 4 0)
(putchar #\m 5 0)
(putchar #\e 6 0)
(putchar #\space 7 0)
(putchar #\t 8 0)
(putchar #\o 9 0)
(loop for x from 10 below 120 do
     (putchar 219 x 0))
(loop for x from 0 below 120 do
     (loop for y from 1 below 38 do
          (putchar (mod (+ (* x 38) y) 256) x y)))

(defun tex-coords-for-code (char-code)
  (declare ((unsigned-byte 8) char-code)
           (optimize (safety 0) (speed 3) (debug 0)))
  (let ((row (floor (/ char-code 32)))
        (col (mod char-code 32)))
    (values (+ (* 9 col) 8)
            (+ (* 16 row) 8))))

(defun draw ()
  (sdl2-ffi.functions:sdl-render-clear *renderer*)
  (loop for x below 120 do
       (loop for y below 38 do
            (multiple-value-bind (tex-coord-x tex-coord-y)
                (tex-coords-for-code (aref *screen-fg* x y))
              (sdl2::c-let ((srect sdl2-ffi::sdl-rect :from *src-rect*)
                            (drect sdl2-ffi::sdl-rect :from *dst-rect*))
                (setf (srect :x) tex-coord-x
                      (srect :y) tex-coord-y
                      (drect :x) (* x 9)
                      (drect :y) (* y 16)))
              ;;(sdl2-ffi.functions:sdl-set-render-draw-color *renderer* 0 0 0 0)
              ;;(sdl2-ffi.functions:sdl-render-fill-rect *renderer* *dst-rect*)
              ;;(sdl2-ffi.functions:sdl-set-texture-color-mod *texture* (random 256) 0 0)
              ;;(sdl2:render-copy *renderer* *texture* :source-rect *src-rect* :dest-rect *dst-rect*)
              (multiple-value-bind (r g b) (color-from-hsv (- (random 50) 25) .75 .75)
                (declare (single-float r g b))
                (sdl2-ffi.functions:sdl-set-texture-color-mod *texture* (floor (* 256 r)) (floor (* 256 g)) (floor (* 256 b))))
              (sdl2:render-copy *renderer* *texture* :source-rect *src-rect* :dest-rect *dst-rect*))))
  (sdl2:render-present *renderer*)
  (incf *draw-count*))

(defun run ()
  (sdl2:with-init (:video)
    (setf *draw-count* 0
          *startup-time-ms* (sdl2:get-ticks))
    (let ((win (sdl2:create-window :title "FiendishRL" :flags '(:shown :resizable :maximized :borderless))))
      
      
      (setf *renderer* (sdl2:create-renderer win -1 '(:accelerated :presentvsync))
            *texture* (sdl2:create-texture *renderer* :argb8888 :static 304 144))

      (read-codepage-into-texture *texture*)
      (sdl2-ffi.functions:sdl-set-texture-alpha-mod *texture* 255)
      (sdl2-ffi.functions:sdl-set-texture-blend-mode *texture* :blend)
            
      ;;(sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+ "nearest")
      #|
      (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-vsync+ "1")
      (sdl2-ffi.functions:sdl-render-set-logical-size ren *width* *height*)
      |#

      (sdl2-ffi.functions:sdl-render-set-logical-size *renderer* (* 9 120) (* 16 38))
      
      (sdl2:with-event-loop (:method :poll)
        (:keydown (:keysym keysym)
                  (let ((scancode (sdl2:scancode-value keysym))
                        (sym (sdl2:sym-value keysym))
                        (mod-value (sdl2:mod-value keysym)))
                    (if (sdl2:scancode= scancode :scancode-w)
                        (if (and
                             (sdl2::mod-value-p mod-value :kmod-rctrl :kmod-lctrl)
                             (sdl2::mod-value-p mod-value :kmod-rshift :kmod-lshift))
                            (print "BACKWARD")
                            (print "FORWARD"))
                        (progn
                          (print (format nil "Key sym: ~a, code: ~a, mod: ~a"
                                         sym
                                         scancode
                                         mod-value))))))
        (:keyup (:keysym keysym)
                (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                  (sdl2:push-event :quit)))
        ;;         (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
        ;;                       ;;(draw-stuff pixels yrel)
        ;;                       (print (format
        ;;                               nil
        ;;                               "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a"
        ;;                               x xrel y yrel state)))

        (:idle ()
               (draw))
               
        (:quit () t))
      (let ((elapsed (/ (- (sdl2:get-ticks) *startup-time-ms*) 1000)))
        (format t "averaged fps = ~a~%" (float (/ *draw-count* elapsed))))
      (sdl2:destroy-texture *texture*)
      (sdl2-ffi.functions:sdl-destroy-renderer *renderer*)
      (sdl2:destroy-window win))))