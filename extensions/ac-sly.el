;;;; Auto-complete for SLY
(require 'ac-sly)
(add-hook 'sly-mode-hook (lambda () (set-up-sly-ac t)))
(add-hook 'sly-mrepl-mode-hook (lambda () (set-up-sly-ac t)))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'sly-mrepl-mode))

