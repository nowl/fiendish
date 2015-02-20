(in-package :fiendish-rl)

(defparameter *draw-count* 0)
(defparameter *startup-time-ms* 0)

(defparameter *text-draw-row* 0.0)

(defun draw ()
  (loop for x below 120 do
              (loop for y below 38 do
                   (fiendish-rl.ffi:putchar x y (char-code #\@) 0.0 (random 0.7) (random .9) 0.0 0.0 0.0)))
  (setf *text-draw-row*
        (if (> *text-draw-row* 30) 10 (+ 0.1 *text-draw-row*)))
  (let ((the-string (format nil "This is a bit of a {(chsv ~f .74 .8)}longer{(creset)} test. We'll see if we can get a few lines of text this way." (* 2 *text-draw-row*))))
    (draw-text the-string 1 (round *text-draw-row*) 50)))

(defun gameloop ()
  (setf *startup-time-ms* (fiendish-rl.ffi:getticks)
        *draw-count* 0)
  (let ((running t))
    (loop while running do
         (destructuring-bind (result key mod) (fiendish-rl.ffi:pollevent)
           (when (and result (= key (char-code #\escape))) (setf running nil)))
         (draw)
         (fiendish-rl.ffi:draw)
         (incf *draw-count*)))

  (let* ((end-time-ms (fiendish-rl.ffi:getticks))
         (elapsed-time (- end-time-ms *startup-time-ms*))
         (average-fps (float (/ (* 1000 *draw-count*) elapsed-time))))
    (format t "frames drawn = ~a~%" *draw-count*)
    (format t "elapsed time = ~a~%" elapsed-time)
    (format t "average fps = ~a~%" average-fps)))

(defun run ()
  (unwind-protect (progn (fiendish-rl.ffi:init)
                         (gameloop))
    (fiendish-rl.ffi:destroy)))

#|
  (sdl2:with-init (:video)
    (setf *draw-count* 0
          *startup-time-ms* (sdl2:get-ticks))
    (let ((win (sdl2:create-window :title "FiendishRL" :flags *sdl-flags* :w *width* :h *height*)))
      
      
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
#|
