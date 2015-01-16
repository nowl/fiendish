(in-package :fiendish-rl)

(defun color-from-hsv (h s v)
  (let* ((c (* v s))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- v c)))
    (case (round (/ h 60))
      (0 (values (+ c m) (+ x m) m))
      (1 (values (+ x m) (+ c m) m))
      (2 (values m (+ c m) (+ x m)))
      (3 (values m (+ x m) (+ c m)))
      (4 (values (+ x m) m (+ c m)))
      (5 (values (+ c m) m (+ x m))))))


    switch( (int)(h / 60) ) {
    case 0: return Color{c+m,x+m,m};
    case 1: return Color{x+m,c+m,m};
    case 2: return Color{m,c+m,x+m};
    case 3: return Color{m,x+m,c+m};
    case 4: return Color{x+m,m,c+m};
    case 5: return Color{c+m,m,x+m};
    }

    return Color{0,0,0};
}
