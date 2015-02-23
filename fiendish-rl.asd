;;; -*- lisp -*-

(defsystem fiendish-rl
  :description "Fiendish RL"
  :long-description ""
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on ("cffi")
  :components
  ((:file "package")
   (:file "core")
   (:file "ffi")
   (:file "data")
   (:file "drawing")
   (:file "game"))
  :serial t)
  
