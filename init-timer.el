(defvar *timer* 0)
(defvar *timer-start-time* nil)

(defun timer/time-to-s (time)
  (/ (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000)
        (car (cdr (cdr time))))
     1000000.0))

(defun timer/start ()
  (setq *timer-start-time* (current-time)))

(defun timer/stop ()
  (if *timer-start-time*
      (incf *timer* (- (timer/time-to-s (current-time))
                       (timer/time-to-s *timer-start-time*)))))

(defun timer/reset ()
  (setq *timer-start-time* nil)
  (setq *timer* 0))

(defun timer/display-timing ()
  (message "------------------------------------------")
  (let ((result *timer*))
    (if *timer-start-time*
        (message "Emacs IDE loaded in %.2f seconds" result)
        (message "Emacs IDE loading time is unknown"))))

