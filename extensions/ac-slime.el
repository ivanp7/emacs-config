;;;; Auto-complete for SLIME
(require 'ac-slime)
(add-hook 'slime-mode-hook (lambda () (set-up-slime-ac t)))
(add-hook 'slime-repl-mode-hook (lambda () (set-up-slime-ac t)))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
