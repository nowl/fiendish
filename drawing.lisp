(in-package :fiendish-rl)

(defparameter *draw-color-fg-stack* nil)

(defparameter *draw-color-fg* (make-color :r 1.0 :g 1.0 :b 1.0))
(defparameter *draw-color-bg* (make-color :r 0.0 :g 0.0 :b 0.0))

(defun putchar (char-or-code x y r g b)
  (let ((char (etypecase char-or-code
                (fixnum char-or-code)
                (character (char-code char-or-code)))))
    (fiendish-rl.ffi:putchar x y char
                             (color-r *draw-color-fg*)
                             (color-g *draw-color-fg*)
                             (color-b *draw-color-fg*)
                             (color-r *draw-color-bg*)
                             (color-g *draw-color-bg*)
                             (color-b *draw-color-bg*))))

(defun draw-text (text start-col start-row end-col &key (draw-text t))
  "Draw the text to the specified spot on the screen. Returns the
number of rows the text took up."
  (declare ((or string list) text)
           (fixnum start-col start-row end-col))
  (setf text (etypecase text
               (string (compile-to-text-cmds text))
               (list text)))
  (loop with row = start-row with col = start-col for cmd in text do
       (let ((cols-remaining (1+ (- end-col col))))
         (when (<= (- cols-remaining (text-command-chars-to-break cmd)) 0)
           (incf row)
           (setf col start-col))
         (ecase (text-command-type cmd)
           (:char
            (let ((the-char (text-command-data cmd)))
              (unless (and (whitespacep the-char)
                           (or
                            ;; test condition of beginning of line and
                            ;; it's a whitespace character
                            (= col start-col)
                            ;; text condition where we would draw a
                            ;; whitespace character at the end of the
                            ;; line
                            (<= cols-remaining 0)))
                (when draw-text
                  (putchar the-char col row 
                           (color-r *draw-color-fg*)
                           (color-g *draw-color-fg*)
                           (color-b *draw-color-fg*))
                  (incf col)))))
           (:color (push *draw-color-fg* *draw-color-fg-stack*)
                   (setf *draw-color-fg* (car (text-command-data cmd))))
           (:creset (setf *draw-color-fg* (pop *draw-color-fg-stack*)))))))
  ;; TODO return number of rows drawn (1+ (- row start-row))))
