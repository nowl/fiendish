(in-package :fiendish-rl)

(defstruct color
  (r 0.0 :type float)
  (g 0.0 :type float)
  (b 0.0 :type float))

(defun color-from-hsv (h s v)
  (declare (optimize (safety 0) (speed 3) (debug 0))
           (single-float s v)
           ((signed-byte 16) h))
  (loop while (< h 0) do (setf h (+ h 360)))           
  (let* ((c (* v s))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- v c)))
    (case (floor (/ h 60))
      (0 (make-color :r (+ c m) :g (+ x m) :b m))
      (1 (make-color :r (+ x m) :g (+ c m) :b m))
      (2 (make-color :r m :g (+ c m) :b (+ x m)))
      (3 (make-color :r m :g (+ x m) :b (+ c m)))
      (4 (make-color :r (+ x m) :g m :b (+ c m)))
      (5 (make-color :r (+ c m) :g m :b (+ x m))))))

;;;; text processing

(defstruct text-command
  ;; either :char, :color, :color-reset
  type
  ;; if type is :char, the character
  ;; if type is :color, the color as a set of values
  data
  ;; number of characters until the next break
  chars-to-break)

(defun read-three-values (str)
  (multiple-value-bind (h pos1)
      (read-from-string str)
    (multiple-value-bind (s pos2)
        (read-from-string str t :eof :start pos1)
      (let ((v (read-from-string str t :eof :start pos2)))
        (values h s v)))))

(defun parse-command (command-str)
  "Returns text-command of parsed command-str."
  (declare (string command-str))
  (cond
    ;; NOTE: these need to be in increasing order of subsequence
    ;; length
    ((string= (subseq command-str 0 4) "chsv")
     (multiple-value-bind (h s v) (read-three-values (subseq command-str 4))
       (make-text-command :type :color :data (multiple-value-list (color-from-hsv h s v)))))
    ((string= (subseq command-str 0 11) "color-reset")
     (make-text-command :type :color-reset))    
    (t (error "can't parse text command."))))
   
(defun whitespacep (char)
  (declare (character char))
  (or (char= #\space char) (char= #\tab char) (char= #\newline char)))

(defun compile-to-text-cmds (text)
  "Converts text into text commands. Returns a list of text-command."
  (declare (string text))
  (let ((cmds
         (loop for pos below (length text) collecting
              (cond
                ((and (char= (char text pos) #\{)
               (char= (char text (1+ pos)) #\())
                 ;; starting a command
                 (let* ((cmd-end-pos (position #\) text
                                               :start (+ pos 2)
                                               :test #'char=))
                        (command (subseq text (+ 2 pos) cmd-end-pos)))
                   (setf pos (+ cmd-end-pos 1))
            (parse-command command)))
                (t (make-text-command :type :char :data (char text pos)))))))
    (loop with count for cmd in (reverse cmds) do
         (cond
           ;; it's whitespace
           ((and count
                 (eq :char (text-command-type cmd))
                 (whitespacep (text-command-data cmd)))
            (setf (text-command-chars-to-break cmd) count
                  count 0))
           ;; it's not whitespace
           ((and count
                 (eq :char (text-command-type cmd)))
            (setf (text-command-chars-to-break cmd) count)
            (incf count))
           ;; it's a command
           (count
            (setf (text-command-chars-to-break cmd) count))
           ;; it's the end of the line
           (t (setf (text-command-chars-to-break cmd) 0
                    count 1))))
    cmds))
