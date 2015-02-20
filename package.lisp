(defpackage :rng
  (:use :cl))

(defpackage :fiendish-rl.ffi
  (:use :cl
        :cffi)
  (:export init
           draw
           getticks
           putchar
           pollevent
           destroy))

(defpackage :fiendish-rl
  (:use :cl))
