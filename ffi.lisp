(in-package :fiendish-rl.ffi)

(push #p"./" cffi:*foreign-library-directories*)

(define-foreign-library fiend
  (t (:default "libfiendish")))

(defcfun ("sdl_init" sdl-init)
    :void
  (program-name :string)
  (width :int)
  (height :int)
  (logical-width :int)
  (logical-height :int))

(defcfun ("set_texture_src" sdl-set-texture-source)
    :void
  (image-filename :string))


(defcfun ("sdl_draw" draw)
  :void)

(defcfun ("sdl_getticks" getticks)
  :int32)

(defcfun ("sdl_blit" sdl-blit)
    :void
  (src-x :int)
  (src-y :int)
  (src-w :int)
  (src-h :int)
  (dst-x :int)
  (dst-y :int)
  (dst-w :int)
  (dst-h :int))

(defcfun ("sdl_pollevent" sdl-pollevent)
    :int32
  (keycode (:pointer :int32))
  (keymod (:pointer :uint16)))

(defcfun ("sdl_destroy" sdl-destroy)
  :void)

(defcfun ("open_font" open-font)
    :pointer
  (filename :string)
  (ptsize :int))

(defcfun ("draw_text" draw-text)
    :void
  (font :pointer)
  (text :string)
  (x :int)
  (y :int)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defun init ()
  (use-foreign-library fiend)
  (sdl-init "fiendish" 800 600 800 600))

(defun destroy ()
  (sdl-destroy)
  (close-foreign-library 'fiend))

(defparameter *keycode* (foreign-alloc :int32))
(defparameter *keymod* (foreign-alloc :uint16))

(defun pollevent ()
  (let ((result (sdl-pollevent *keycode* *keymod*)))
    (if (= result 0)
        (list 0 0 0)
        (list result
              (mem-ref *keycode* :int32)
              (mem-ref *keymod* :uint16)))))
       

;;(use-foreign-library fiend)

;;(sdl-init)
;;(sdl-destroy)


;;(close-foreign-library 'fiend)
#|
(sdl-init)
(loop for x below 10 do
     (loop for y below 10 do
          (sdl-putchar x y (char-code #\@) 1.0 1.0 1.0 0.0 0.0 0.0)))

(sdl-draw)
(sdl-destroy)

(sdl-pollevent *keycode* *keymod*)
;;(format t "~a ~a~%" (mem-ref *keycode* :int32) (mem-ref *keymod* :uint16))

(close-foreign-library 'fiend)
|#
