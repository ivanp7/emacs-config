;;;; NLinum mode

;; Permanently disable linum-mode to avoid conflicts with nlinum-mode
(define-minor-mode linum-mode
    ""
  :lighter ""
  (setq linum-mode nil))

(global-nlinum-mode)

;; Select entire line by clicking at line number
(defvar *linum-mdown-line* nil)

(defun md-select-linum (event)
  (interactive "e")
  (mouse-set-point event)
  (goto-line (nlinum--line-number-at-pos))
  (set-mark (point))
  (setq *linum-mdown-line* (nlinum--line-number-at-pos)))

(defun mu-select-linum (event)
  (interactive "e")
  (when *linum-mdown-line*
    (let (mu-line)
      (mouse-set-point event)
      (setq mu-line (nlinum--line-number-at-pos))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown-line* nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; Preset width nlinum
(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum--width
                  (length (number-to-string
                           (count-lines (point-min) (point-max)))))))

;; Fix 'nlinum--face-width: Invalid face: linum' error
(defvar frame-ready nil)
(add-hook 'after-make-frame-functions
          (lambda (frame) (set-frame-parameter frame 'frame-ready t)))
(add-hook 'after-init-hook (lambda () (set-frame-parameter nil 'frame-ready t)))

(defun nlinum--setup-window ()
  (let ((width (if (and frame-ready (display-graphic-p)) ;; <-- Here
                   (ceiling
                    (let ((width (nlinum--face-width 'linum)))
                      (if width
                          (/ (* nlinum--width 1.0 width)
                             (frame-char-width))
                          (/ (* nlinum--width 1.0
                                (nlinum--face-height 'linum))
                             (frame-char-height)))))
                   nlinum--width)))
    (set-window-margins nil (if nlinum-mode width)
                        (cdr (window-margins)))))
