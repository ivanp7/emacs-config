;;;; Auto indentation
;;(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode)
(auto-indent-global-mode)

(setq auto-indent-backward-delete-char-behavior 'untabify)
