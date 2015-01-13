(in-package :rng)

(export '(norm int init choice))

;; Algorithm adapted from Complementary-Multiply-with-Carry which has
;; an insanely long period.. 2^131104.
;; Details here.. http://en.wikipedia.org/wiki/Multiply-with-carry

(defparameter *Q* (make-array '(4096) :element-type '(unsigned-byte 32) :initial-element 0))
(defparameter *it* 4095)
(defconstant +PHI+ #x9e3779b9)
(defparameter *c* 362436)

(declaim ((unsigned-byte 32) *it* *c*)
         ((simple-array (unsigned-byte 32)) *Q*))

(defun init (x)
  "Set the random number seed. Must be an integer from [0, 2^32)."
  (declare ((unsigned-byte 32) x))
  (setf
   *c* 362436
   *it* 4095
   (aref *Q* 0) (logand #xffffffff x)
   (aref *Q* 1) (logand #xffffffff (+ x +PHI+))
   (aref *Q* 2) (logand #xffffffff (+ x +PHI+ +PHI+)))
  (loop for i fixnum from 3 below 4096 do
       (setf (aref *Q* i)
             (logxor (aref *Q* (- i 3))
                     (aref *Q* (- i 2))
                     +PHI+
                     i))))

(defun rand-cmwc ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf *it* (logand 4095 (1+ *it*)))
  (let ((tmp (logand #xffffffffffffffff
                     (+ (* 18705 (aref *Q* *it*)) *c*))))
    (declare ((unsigned-byte 64) tmp))
    (setf *c* (ash tmp -32)
          (aref *Q* *it*) (logand #xffffffff
                                  (- #xfffffffe tmp))))
  (aref *Q* *it*))

(defun int ()
  "random integer in the range [0, 2^32)"
  (rand-cmwc))

(defun norm ()
  "random value in the range [0, 1)"
  (float (/ (rand-cmwc) (expt 2 32))))

(defun choice (list)
  "Returns a random choice from the list."
  (let ((result (mod (int) (length list))))
    (elt list result)))

;; default initialization seed
(init 0)
