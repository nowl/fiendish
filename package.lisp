(defpackage :rng
  (:use :cl))

(defpackage :fiendish-rl.ffi
  (:use :cl
        :cffi)
  (:export init
           draw
           clear
           getticks
           set-texture-source
           open-font
           draw-text
           blit
           pollevent
           destroy))

(defpackage :fiendish-rl
  (:use :cl))
