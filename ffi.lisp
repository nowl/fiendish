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

(defcfun ("set_texture_src" set-texture-source)
    :void
  (image-filename :string))

(defcfun ("sdl_draw" draw)
  :void)

(defcfun ("sdl_clear" clear)
  :void)

(defcfun ("sdl_getticks" getticks)
  :int32)

(defcfun ("sdl_blit" blit)
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

(defcfun ("load_sound" load-sound)
    :pointer
  (file :string))

(defcfun ("play_sound" play-sound)
    :void
  (chunk :pointer))

(defun init (&rest args)
  (use-foreign-library fiend)
  ;;(sdl-init "fiendish" 1280 720 320 200))
  (apply #'sdl-init args))

(defun destroy ()
  (sdl-destroy)
  (close-foreign-library 'fiend))

(defparameter *keycode* (foreign-alloc :int32))
(defparameter *keymod* (foreign-alloc :uint16))

(defun pollevent ()
  (let ((result (sdl-pollevent *keycode* *keymod*)))
    (if (= result 0)
        (list nil 0 0)
        (list (ecase result
                (768 :press)
                (769 :release))
              (mem-ref *keycode* :int32)
              (mem-ref *keymod* :uint16)))))
