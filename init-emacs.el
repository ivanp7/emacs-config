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
(setq-default indent-tabs-mode nil)
(defun format-current-buffer ()
  (indent-region (point-min) (point-max)))
(defun untabify-current-buffer ()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'format-current-buffer)
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
              (replace-match ",@ "))))))

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
(global-set-key (kbd "<C-wheel-up>") '(lambda ()
                                       (interactive)
                                       (scroll-right 2 t)))
(global-set-key (kbd "<C-wheel-down>") '(lambda ()
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
(prefer-coding-system 'windows-1251)
(set-terminal-coding-system 'windows-1251)
(set-keyboard-coding-system 'windows-1251-unix)
(set-selection-coding-system 'windows-1251)
(setq locale-coding-system 'windows-1251)
(set-default-coding-systems 'windows-1251)

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
