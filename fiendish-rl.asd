;;; -*- lisp -*-

(defsystem fiendish-rl
  :description "Fiendish RL"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on ("sdl2")
  :components
  ((:file "package")
   (:file "core")
   (:file "codepage-437")
   (:file "rng")
   (:file "dungeoneer")
   (:file "drawing")
   (:file "game"))
  :serial t)
  
