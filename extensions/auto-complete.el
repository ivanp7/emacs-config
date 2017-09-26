;;;; Auto-complete
(require 'auto-complete)

(setq ac-quick-help-prefer-pos-tip t)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 1.5)
(setq ac-auto-show-menu 0.3)

(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)

(global-auto-complete-mode t)
(add-to-list 'ac-modes 'lisp-mode)

(setq-default ac-sources
              '(ac-source-filename
                ac-source-files-in-current-dir
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers))

(add-hook 'emacs-lisp-mode
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-features)
            (add-to-list 'ac-sources 'ac-source-functions)
            (add-to-list 'ac-sources 'ac-source-symbols)
            (add-to-list 'ac-sources 'ac-source-variables)))
