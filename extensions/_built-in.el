;;;; Hide/Show feature
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(setq hs-minor-mode-menu t)

;;;; Eldoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; Speedbar
(require 'speedbar)
(setq speedbar-show-unknown-files t)
