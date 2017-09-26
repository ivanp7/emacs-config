;;;; Common input configuration

;; Menu key
(defconst menu-key-name
  (cond
    ((system-is-windows) "<apps>")
    ((system-is-linux) "<menu>")))

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

;;;; Installing custom keymaps
(dolist (f (directory-files cl-ide-init-keymaps-path nil
                            "^\\([^_[:space:]]+[[:graph:]]*[.]el\\)$" nil))
  (load (concat cl-ide-init-keymaps-path f)))
