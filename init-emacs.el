
;;;; Basic Emacs tuning
;; needed to be able to rebind <C-return>:
(setq cua-rectangle-mark-key (kbd "C-x j"))

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
 '(default ((t (:family "FreeMono" :foundry "outline" :slant normal
                        :weight normal :height 80 :width normal)))))
;;(set-default-font "DejaVu Sans Mono") ; :height 75
;;(set-default-font "Consolas-8")

;; Window transparency modification function
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 5%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 5) (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;;;; Frame title
(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                 (or (file-remote-p default-directory 'user)
                  user-real-login-name)
                 (or (file-remote-p default-directory 'host)
                  system-name)
                 (buffer-name)
                 (cond
                   (buffer-file-truename
                    (concat "(" buffer-file-truename ")"))
                   (dired-directory
                    (concat "{" dired-directory "}"))
                   (t
                    "[no file]")))))

;;;; Setting up color theme
(load-theme 'granger t)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

;;;; Generic settings
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (menu-bar-mode -1)

(setq initial-scratch-message
      ";; This buffer is for notes you don't want to save, and for Emacs Lisp evaluation.
;; If you want to create a file, visit that file with <Ctrl+O>,
;; then enter the text in that file's own buffer.
;; Press <Ctrl+J> to evaluate expression and print result at the point.
;; For Common Lisp evaluation use the *sl(ime/y)-scratch* buffer instead.\n\n")

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress

;; (setq mouse-autoselect-window 0)

(setq inhibit-startup-message t)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore) ; no sound

(setq font-lock-maximum-decoration t)

(require 'jit-lock)
(setq font-lock-support-mode 'jit-lock-mode) ; needed to speed up font-lock-mode

(defalias 'yes-or-no-p 'y-or-n-p) ; short messages

;; Delete trailing whitespaces, format buffer and untabify when save buffer
(setq indent-buffer-exception-modes '(bibtex-mode))
(setq-default untabify-on-saving nil)
(setq-default indent-tabs-mode nil)

(defun indent-current-buffer ()
  (interactive)
  (unless (member major-mode indent-buffer-exception-modes)
    (indent-region (point-min) (point-max))))

(defun untabify-current-buffer ()
  (interactive)
  (if (not untabify-on-saving)
      (untabify (point-min) (point-max)))
  nil)

(defun current-line-empty-p ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun add-space-in-empty-lines ()
  (interactive)
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
      (let ((on-empty-line (and (eql (char-before (point)) ?\n)
                              (eql (char-after (point)) ?\n))))
        (save-excursion
          (beginning-of-buffer)
          (while (re-search-forward "\n\n" nil t)
            (if (not (member (plist-get (text-properties-at (point)) 'face)
                      '(font-lock-string-face font-lock-comment-face)))
                (replace-match "\n \n"))))
        (if on-empty-line
            (right-char)))))

(defun comma-at-sign-remove-spaces ()
  (interactive)
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward ",@[ \t]+" nil t)
          (if (not (member (plist-get (text-properties-at (point)) 'face)
                    '(font-lock-string-face font-lock-comment-face)))
              (replace-match ",@"))))))

(defun prepare-buffer-for-saving ()
  (interactive)
  (unless (eql major-mode 'fundamental-mode)
    (untabify-current-buffer)
    (delete-trailing-whitespace)
    (add-space-in-empty-lines)
    (indent-current-buffer)
    (comma-at-sign-remove-spaces)))

(add-to-list 'write-file-functions 'prepare-buffer-for-saving)

(global-auto-revert-mode 1)

;; A ",@symbol" highlight bug workaround:
;; insert one space between ",@" and a symbol in code
;; (except for strings and comments)
;; do not insert space between ",@" and "("
(defun comma-at-sign-add-spaces ()
  (interactive)
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
      (save-excursion
        (comma-at-sign-remove-spaces)
        (beginning-of-buffer)
        (while (re-search-forward ",@" nil t)
          (if (and (not (member (plist-get (text-properties-at (point)) 'face)
                       '(font-lock-string-face font-lock-comment-face)))
                 (not (member (char-after (point)) '(?\( ?\, ?\` ?\'))))
              (replace-match ",@"))))))

;; End of file newlines
(setq require-final-newline t) ; add newline to the end of file when saving
(setq next-line-add-newlines nil) ; don't add newline when moving point by arrow keys

(setq truncate-lines nil)

;;; Configure backups and autosaves
(setq-default make-backup-files t)
(setq-default auto-save-default t)

;;; Scrolling settings
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)

;; scroll five lines at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 15))) ;; five lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq auto-hscroll-mode nil)
(setq automatic-hscrolling nil)
(setq hscroll-margin 1)
(setq hscroll-step 1)

(setq next-screen-context-lines 25)

;;;; Coding systems
(cond
  ((system-is-windows)
   (prefer-coding-system 'windows-1251)
   (set-terminal-coding-system 'windows-1251)
   (set-keyboard-coding-system 'windows-1251-unix)
   (set-selection-coding-system 'windows-1251)
   (setq locale-coding-system 'windows-1251)
   (set-default-coding-systems 'windows-1251))
  ((system-is-linux)
   (set-language-environment "UTF-8")
   (prefer-coding-system 'utf-8)
   (set-terminal-coding-system 'utf-8)
   (set-keyboard-coding-system 'utf-8)
   (set-selection-coding-system 'utf-8)
   (setq locale-coding-system 'utf-8)
   (set-default-coding-systems 'utf-8)))

;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

;;;; Input method
(setq default-input-method "russian-computer")

;;;; Displaying buffers
(setq display-buffer-alist
      '(("\\*inferior-lisp\\*" display-buffer-same-window (nil))))

;;;; Window refresh function
(defun refresh-window ()
  (interactive)
  (force-window-update (get-buffer-window))
  (redisplay))

;;;; Frame maximization function
(defun maximize-frame ()
  (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
      (modify-frame-parameters nil `((maximized . maximized)))
      (modify-frame-parameters nil `((fullscreen . maximized)))))
