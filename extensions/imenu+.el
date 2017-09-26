;;;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t) ;; automatically update list of functions in the buffer
;;(setq imenu-use-popup-menu nil) ; enable Imenu dialogs in the minibuffer only

(defun try-to-add-imenu ()
  (condition-case nil (imenup-add-defs-to-menubar) (error nil))) ; enable iMenu+
;;(condition-case nil (imenu-add-to-menubar "IMenu") (error nil))) ; not iMenu
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)
