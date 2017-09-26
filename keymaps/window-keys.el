(defvar my-window-control-keymap (make-keymap)
  "My window control keymap.")

;;; Buffers and windows configuration control keys
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <backspace>"))
  'kill-this-buffer)

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " "
                 menu-key-name))
  'switch-window)

(defun switch-to-last-window ()
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " SPC"))
  'switch-to-last-window)

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " ,"))
  'previous-buffer)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " ."))
  'next-buffer)
(define-key my-window-control-keymap
    (kbd "C-,")
  'previous-buffer)
(define-key my-window-control-keymap
    (kbd "C-.")
  'next-buffer)
;; (define-key my-window-control-keymap (kbd (concat menu-key-name " <"))
;;   (lambda () (interactive) (other-window 1) (previous-buffer) (other-window 1)))
;; (define-key my-window-control-keymap (kbd (concat menu-key-name " >"))
;;   (lambda () (interactive) (other-window 1) (next-buffer) (other-window 1)))

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <left>"))
  'buf-move-left)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <S-left>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap)
     (buf-move-left)
     (setq buffer-move-behavior 'move)))
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <up>"))
  'buf-move-up)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <S-up>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap)
     (buf-move-up)
     (setq buffer-move-behavior 'move)))
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <right>"))
  'buf-move-right)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <S-right>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap)
     (buf-move-right)
     (setq buffer-move-behavior 'move)))
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <down>"))
  'buf-move-down)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <S-down>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap)
     (buf-move-down)
     (setq buffer-move-behavior 'move)))

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " C-<left>"))
  'split-window-horizontally)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " C-<up>"))
  'split-window-vertically)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " C-<right>"))
  'split-window-right)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " C-<down>"))
  'split-window-below)
(define-key my-window-control-keymap
    (kbd (concat menu-key-name " <delete>"))
  'delete-window)

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " `")) 'balance-windows)

(defun make-new-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    ;;(unless (one-window-p)
    ;;  (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(define-key my-window-control-keymap
    (kbd (concat menu-key-name " RET"))
  'make-new-frame)

;;;; Installing minor mode for keys
(define-minor-mode my-window-control-keymap-mode
    "A minor mode so that my key settings override annoying major modes."
  t " my-window-keys" my-window-control-keymap)
