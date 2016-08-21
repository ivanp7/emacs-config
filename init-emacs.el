
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
  '(ac-slime auto-complete auto-indent-mode buffer-move cursor-chg expand-region
    highlight-stages highlight-symbol hl-sexp imenu+ magic-latex-buffer magit
    nlinum org paren-face pos-tip pretty-mode pretty-symbols rainbow-identifiers
    slime switch-window tabbar undo-tree)
  "a list of packages to ensure are installed at launch.")

;;;; Initializing package manager and loading useful packages
(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in *required-packages*
     when (not (package-installed-p p)) do (return nil)
     finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p *required-packages*)
    (when (not (package-installed-p p))
      (package-install p))))

;;;; Generic settings
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (menu-bar-mode -1)

(setq initial-scratch-message
      ";; This buffer is for notes you don't want to save, and for Emacs Lisp evaluation.
;; If you want to create a file, visit that file with <Ctrl+O>,
;; then enter the text in that file's own buffer.
;; Press <Ctrl+J> to evaluate expression and print result at the point.
;; For Common Lisp evaluation use the *slime-scratch* buffer instead.\n\n")

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress

(setq mouse-autoselect-window 0)

(setq inhibit-startup-message t)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore) ; no sound

(setq font-lock-maximum-decoration t)

(require 'jit-lock)
(setq font-lock-support-mode 'jit-lock-mode) ; needed to speed up font-lock-mode

(defalias 'yes-or-no-p 'y-or-n-p) ; short messages

;; Delete trailing whitespaces, format buffer and untabify when save buffer
(setq indent-buffer-exception-modes '(bibtex-mode))
(setq-default indent-tabs-mode nil)
(defun indent-current-buffer ()
  (unless (member major-mode indent-buffer-exception-modes)
    (indent-region (point-min) (point-max))))
(defun untabify-current-buffer ()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'indent-current-buffer)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(defun comma-at-sign-remove-spaces ()
  (interactive)
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward ",@[ \t]+" nil t)
          (if (not (member (plist-get (text-properties-at (point)) 'face)
                           '(font-lock-string-face font-lock-comment-face)))
              (replace-match ",@"))))))
(add-to-list 'write-file-functions 'comma-at-sign-remove-spaces)

;; A ",@symbol" highlight bug workaround:
;; insert one space between ",@" and a symbol in code (except for strings and comments)
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
(setq next-line-add-newlines nil) ; do not add newline when moving point by arrow keys

(setq truncate-lines nil)

;;; Configure backups and autosaves
(setq-default make-backup-files nil)
(setq-default auto-save-defaults t)

;;;; Scrolling settings
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

(setq next-screen-context-lines 30)

;; Ignoring mouse horizontal wheeling
(global-set-key (kbd "M--") '(lambda ()
                              (interactive)
                              (scroll-right 2 t)))
(global-set-key (kbd "M-=") '(lambda ()
                              (interactive)
                              (scroll-left 2 t)))

(global-set-key (kbd "<wheel-right>") 'ignore)
(global-set-key (kbd "<double-wheel-right>") 'ignore)
(global-set-key (kbd "<triple-wheel-right>") 'ignore)
(global-set-key (kbd "<wheel-left>") 'ignore)
(global-set-key (kbd "<double-wheel-left>") 'ignore)
(global-set-key (kbd "<triple-wheel-left>") 'ignore)
(global-set-key (kbd "<S-wheel-right>") 'ignore)
(global-set-key (kbd "<S-double-wheel-right>") 'ignore)
(global-set-key (kbd "<S-triple-wheel-right>") 'ignore)
(global-set-key (kbd "<S-wheel-left>") 'ignore)
(global-set-key (kbd "<S-double-wheel-left>") 'ignore)
(global-set-key (kbd "<S-triple-wheel-left>") 'ignore)
(global-set-key (kbd "<C-wheel-right>") 'ignore)
(global-set-key (kbd "<C-double-wheel-right>") 'ignore)
(global-set-key (kbd "<C-triple-wheel-right>") 'ignore)
(global-set-key (kbd "<C-wheel-left>") 'ignore)
(global-set-key (kbd "<C-double-wheel-left>") 'ignore)
(global-set-key (kbd "<C-triple-wheel-left>") 'ignore)

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

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)

;;;; Input method
(setq default-input-method "russian-computer")
