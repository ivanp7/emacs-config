;;;; Highlight matching parens
(set-face-background 'show-paren-match "red") ;; "steelblue4"
(set-face-foreground 'show-paren-match "white")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(set-face-background 'show-paren-mismatch "yellow")
(set-face-foreground 'show-paren-mismatch "white")
(set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold)

(set-face-background 'region "SteelBlue3")

;;;; Hide/Show feature
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(setq hs-minor-mode-menu t)

;;;; Eldoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; Speedbar
(require 'speedbar)
(setq speedbar-show-unknown-files t)
