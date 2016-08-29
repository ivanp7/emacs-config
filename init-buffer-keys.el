(defvar my-buffer-keys-minor-mode-map (make-keymap) "my-buffer-keys-minor-mode keymap.")

;;; Buffers and windows configuration control keys
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <backspace>"))
  'kill-this-buffer)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <S-backspace>"))
  (lambda () (interactive) (other-window 1) (kill-this-buffer) (other-window 1)))

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " "
                                                       menu-key-name)) 'switch-window)

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " ,")) 'previous-buffer)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " .")) 'next-buffer)
(define-key my-buffer-keys-minor-mode-map (kbd "C-,") 'previous-buffer)
(define-key my-buffer-keys-minor-mode-map (kbd "C-.") 'next-buffer)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <"))
  (lambda () (interactive) (other-window 1) (previous-buffer) (other-window 1)))
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " >"))
  (lambda () (interactive) (other-window 1) (next-buffer) (other-window 1)))

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <left>"))
  'buf-move-left)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <S-left>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-left) (setq buffer-move-behavior 'move)))
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <up>"))
  'buf-move-up)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <S-up>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-up) (setq buffer-move-behavior 'move)))
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <right>"))
  'buf-move-right)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <S-right>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-right) (setq buffer-move-behavior 'move)))
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <down>"))
  'buf-move-down)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <S-down>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-down) (setq buffer-move-behavior 'move)))

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " C-<left>"))
  'split-window-horizontally)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " C-<up>"))
  'split-window-vertically)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " C-<right>"))
  'split-window-right)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " C-<down>"))
  'split-window-below)
(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " <delete>"))
  'delete-window)

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " `")) 'balance-windows)

(define-key my-buffer-keys-minor-mode-map (kbd (concat menu-key-name " SPC"))
  '(lambda ()
    (interactive)
    (let ((buffer (current-buffer)))
      ;;(unless (one-window-p)
      ;;  (delete-window))
      (display-buffer-pop-up-frame buffer nil))))

;;;; Installing minor mode for keys
(define-minor-mode my-buffer-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
  t " myk0" 'my-buffer-keys-minor-mode-map)
