(in-package :fiendish-rl)

(defun run ()
  (unwind-protect
       (progn
         (cffi:use-foreign-library libtos)
         (tos-init)
         (setf *total-frames* 0)
         (let ((start-time (tos-getticks))
               (next-update (tos-getticks)))
           (setf *game-running* t)
           (loop while *game-running* do
                (cffi:with-foreign-objects ((key-p :int32) (keymod-p :uint16))
                  (when (plusp (tos-poll-event key-p keymod-p))
                    (let ((key (cffi:mem-ref key-p :int32))
                          (keymod (cffi:mem-ref keymod-p :uint16)))
                      (handle-input key keymod))))
                (loop for loops fixnum from 0 
                   while (and (> (tos-getticks) next-update)
                              (< loops *max-frame-skips*)) do
                     (incf next-update *ms-per-tick*)
                     (update)
                     (incf *total-ticks*))
                (render)         
                (tos-draw)
                (incf *total-frames*))
           (format t "total game ticks ~a~%" *total-ticks*)
           (format t "frames drawn ~a~%" *total-frames*)
           (let ((elapsed-time (- (tos-getticks) start-time)))
             (format t "elapsed time ~a s~%" (/ elapsed-time 1000.0))
             (format t "average fps ~a~%" (/ *total-frames* (/ elapsed-time 1000.0))))))

    (tos-cleanup)
    (cffi:close-foreign-library 'libtos)))
