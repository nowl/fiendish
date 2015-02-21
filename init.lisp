(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/nowl/quicklisp/setup.lisp"))

(ql:quickload :cffi)

(require 'asdf)
;;(push #p"/home/nowl/dev/fiendish/" asdf:*central-registry*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #p"./" asdf:*central-registry*)
  (require :fiendish-rl))

(push #p"/home/nowl/dev/fiendish/" cffi:*foreign-library-directories*)

(swank:set-package :fiendish-rl)

(time (fiendish-rl::run))

