(in-package :fiendish-rl)

(defparameter *height* 720)
(defparameter *width* (round (/ (* *height* 16) 9)))


(defun run ()
  (sdl2:with-init (:video :audio)
    (setf *draw-count* 0
          *startup-time-ms* (sdl2:get-ticks))
    (let* ((win (sdl2:create-window :title "FiendishRL" :flags '(:shown :resizable :maximized :borderless)))
           (ren (sdl2:create-renderer win -1 '(:accelerated :presentvsync)))
           (texture (sdl2:create-texture ren :argb8888 :static 304 144)))

      (read-codepage-into-texture texture)
            
      #|
      (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+ "nearest")
      (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-vsync+ "1")
      (sdl2-ffi.functions:sdl-render-set-logical-size ren *width* *height*)
      |#

      (sdl2-ffi.functions:sdl-render-set-logical-size ren 1080 608)

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
        (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                      ;;(draw-stuff pixels yrel)
                      (print (format
                              nil
                  "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a"
                  x xrel y yrel state)))
        (:idle ()
               
               ;;(sdl2-ffi.functions:sdl-update-texture tex (cffi-sys:null-pointer) pixels (* *width* (cffi:foreign-type-size :uint32)))
               (sdl2-ffi.functions:sdl-render-clear ren)
               ;(draw-stuff ren)
               ;;(sdl2-ffi.functions:sdl-render-copy ren tex (cffi-sys:null-pointer)
               ;;                                    (cffi-sys:null-pointer))

               (sdl2:render-copy ren texture)
               (sdl2:render-present ren))
        (:quit () t))
      (sdl2:destroy-texture texture)
      (sdl2-ffi.functions:sdl-destroy-renderer ren)
      (sdl2:destroy-window win))))
