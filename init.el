;;;; Starting countdown of "init.el" loading time
(defvar *emacs-load-start* (current-time))

(defun anarcat/time-to-s (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))

(defun anarcat/display-timing ()
  (message "------------------------------------------")
  (message "Emacs loaded in %f seconds" (/ (- (anarcat/time-to-s (current-time))
                                              (anarcat/time-to-s *emacs-load-start*)) 1000000.0)))

;;;; Setting personal data
(setq user-full-name   "ivanp7")
(setq user-mail-adress "ivanp7@mail.ru")

;;;; Setting up load-path and default-directory
(setq-default default-directory "d:/lisp/ivanp7/")
(setq default-directory "d:/lisp/ivanp7/")
(add-to-list 'load-path "./init/")
(add-to-list 'load-path "./init/elisp/")

;;;; System-type definition functions
(defun system-is-linux ()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
  (string-equal system-type "windows-nt"))

;;;; Basic Emacs tuning
(setq cua-rectangle-mark-key (kbd "C-x j")) ; needed to be able to rebind <C-return>
(custom-set-variables
 '(cua-mode t nil (cua-base)) ; use CUA keys (Ctrl+C, Ctrl+X, Ctrl+V)
 '(save-place t nil (saveplace)) ; save places in files between sessions
 '(show-paren-mode t) ; highlight matching parentheses
 '(global-hl-line-mode nil) ; highlight current line
 '(global-linum-mode nil) ; show line numbers
 '(column-number-mode t) ; show column of the point
 '(size-indication-mode t) ; show file size
 '(org-replace-disputed-keys t))
(custom-set-faces
 '(default ((t (:family "Anonymous Pro" :foundry "outline" :slant normal
                        :weight normal :height 80 :width normal)))))
;;(set-default-font "DejaVu Sans Mono") ; :height 75
;;(set-default-font "Consolas-8")

(add-hook 'window-setup-hook 'toggle-frame-maximized t) ; always maximize window on startup

;; Window transparency modification function
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 5%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 5) (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;;;; Setting up color theme
(add-to-list 'custom-theme-load-path "./init/color-themes/")
(load-theme 'granger t)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

;;;; Advanced Emacs tuning
(defvar *required-packages*
  '(ac-slime auto-complete auto-indent-mode buffer-move expand-region
    highlight-stages highlight-symbol hl-sexp imenu+ magit magic-latex-buffer
    nlinum paren-face pos-tip pretty-mode pretty-symbols slime
    tabbar undo-tree)
  "a list of packages to ensure are installed at launch.")

(load "init-emacs.el")

;;;; Initializing extensions
(load "init-extensions.el")

;;;; Setting up a minor mode for useful key shortcuts
(load "init-keys.el")

;;;; Finally, splitting screen and starting SLIME
(split-window-horizontally)
(slime)
