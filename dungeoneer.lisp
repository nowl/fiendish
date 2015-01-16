(in-package :fiendish-rl)

;; mobs

(defparameter *monster-catalog*
  '(((:name "rat") (:symbol #\r) (:color (:hsv 30 .75 .75)))
    ((:name "screamer") (:symbol #\s) (:color (:hsv 120 .75 .75)))
    ((:name "skeleton") (:symbol #\S) (:color (:hsv 0 0 .8)))))

(defun get-monster-prop (prop monster-name)
  "Retrieves given property from the *monster-catalog*"
  (let ((entry (find monster-name *monster-catalog*
                     :test #'string-equal
                     :key #'(lambda (row)
                              (cadr (assoc :name row))))))
    (cadr (assoc prop entry))))
