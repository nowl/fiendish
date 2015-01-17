(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/nowl/quicklisp/setup.lisp"))

(ql:quickload :swank) ;; XXX: not sure why this is required by sdl2?

(ql:quickload :sdl2)
;(quit)

(require 'asdf)
;;(push #p"/home/nowl/dev/fiendish/" asdf:*central-registry*)
(push #p"./" asdf:*central-registry*)
(asdf:oos 'asdf:load-op 'fiendish-rl)

(time (fiendish-rl::run))
(quit)
