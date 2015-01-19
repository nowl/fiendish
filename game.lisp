(in-package :fiendish-rl)

(defparameter *width* 1920)
(defparameter *height* (floor (/ (* *width* 9) 16)))

(defparameter *src-rect* (sdl2:make-rect 0 0 9 16))
(defparameter *dst-rect* (sdl2:make-rect 0 0 9 16))
(defparameter *renderer* nil)
(defparameter *texture* nil)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

(loop for x from 0 below 120 do
     (loop for y from 1 below 38 do
          (putchar 219 x y 1.0 0.0 0.0)))
   ;;(putchar (mod (+ (* x 38) y) 256) x y)))
       
(draw-text "This is a bit of a longer test. We'll see if we can get a few lines of text this way." 1 10 50)

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
                (tex-coords-for-code (cell-draw-code (aref *screen-fg* x y)))
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
              (let ((color (cell-draw-color (aref *screen-fg* x y))))
                (sdl2-ffi.functions:sdl-set-texture-color-mod *texture* (floor (* 255 (color-r color))) (floor (* 255 (color-g color))) (floor (* 255 (color-b color)))))
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
