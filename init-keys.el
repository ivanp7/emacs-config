(defvar my-common-keys-minor-mode-map (make-keymap) "my-common-keys-minor-mode keymap.")

;;; Basic keys
(define-key my-common-keys-minor-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key my-common-keys-minor-mode-map (kbd "C-y") 'undo-tree-redo)

(define-key my-common-keys-minor-mode-map (kbd "C-S-v")
  (lambda () (interactive) (popup-menu 'yank-menu)))

(define-key my-common-keys-minor-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)

;; Needed to disable destroying windows configuration by pressing <escape>
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
         ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(define-key my-common-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)

;; Always open or create file with GUI
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

(define-key my-common-keys-minor-mode-map (kbd "C-o") 'find-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
              (rename-file filename new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil)
              (message "File '%s' successfully renamed to '%s'"
                       name (file-name-nondirectory new-name)))))))

(define-key my-common-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-common-keys-minor-mode-map (kbd "C-S-s") 'write-file)
(define-key my-common-keys-minor-mode-map (kbd "M-s s") 'rename-current-buffer-file)

(define-key my-common-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-common-keys-minor-mode-map (kbd "C-l") 'goto-line)

(define-key my-common-keys-minor-mode-map (kbd "<f2>") 'slime-selector)

;;; Buffers and windows configuration control keys
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <backspace>"))
  'kill-this-buffer)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <S-backspace>"))
  (lambda () (interactive) (other-window 1) (kill-this-buffer) (other-window 1)))

(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " '")) 'other-window)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " ,")) 'previous-buffer)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " .")) 'next-buffer)
(define-key my-common-keys-minor-mode-map (kbd "C-,") 'previous-buffer)
(define-key my-common-keys-minor-mode-map (kbd "C-.") 'next-buffer)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <"))
  (lambda () (interactive) (other-window 1) (previous-buffer) (other-window 1)))
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " >"))
  (lambda () (interactive) (other-window 1) (next-buffer) (other-window 1)))

(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <left>"))
  'buf-move-left)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <S-left>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-left) (setq buffer-move-behavior 'move)))
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <up>"))
  'buf-move-up)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <S-up>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-up) (setq buffer-move-behavior 'move)))
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <right>"))
  'buf-move-right)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <S-right>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-right) (setq buffer-move-behavior 'move)))
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <down>"))
  'buf-move-down)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <S-down>"))
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-down) (setq buffer-move-behavior 'move)))

(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " C-<left>"))
  'split-window-horizontally)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " C-<up>"))
  'split-window-vertically)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " C-<right>"))
  'split-window-right)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " C-<down>"))
  'split-window-below)
(define-key my-common-keys-minor-mode-map (kbd (concat menu-key-name " <delete>"))
  'delete-window)

;;; Commands execution
(define-key my-common-keys-minor-mode-map (kbd "C-M-x") 'shell-command)
(define-key my-common-keys-minor-mode-map (kbd "S-C-M-x") 'repeat-complex-command)

;;;; Search
;;(define-key my-common-keys-minor-mode-map (kbd "M-h") 'highlight-symbol-at-point)
(define-key my-common-keys-minor-mode-map (kbd "M-r") 'highlight-regexp)
(define-key my-common-keys-minor-mode-map (kbd "M-u") 'unhighlight-regexp)

(define-key my-common-keys-minor-mode-map (kbd "C-S-f") 'occur)

;; (define-key my-common-keys-minor-mode-map (kbd "C-S-G <up>") 'isearch-backward)
;; (define-key my-common-keys-minor-mode-map (kbd "M-C-S-G <up>") 'isearch-repeat-backward)
;; (define-key my-common-keys-minor-mode-map (kbd "C-S-G <down>") 'isearch-forward)
;; (define-key my-common-keys-minor-mode-map (kbd "M-C-S-G <down>") 'isearch-repeat-forward)

;;;; Toggle input method
(define-key my-common-keys-minor-mode-map (kbd "C-/") 'toggle-input-method)

;;; Font size control keybindings
(define-key my-common-keys-minor-mode-map (kbd "<C-M-next>") 'text-scale-decrease)
(define-key my-common-keys-minor-mode-map (kbd "<C-M-prior>") 'text-scale-increase)

;;; Transparency control keybindings

;; C-M-i will decrease opacity (== increase transparency
;; C-M-o will increase opacity (== decrease transparency)
;; C-M-p will returns the state to normal
(define-key my-common-keys-minor-mode-map (kbd "C-M-i")
  (lambda () (interactive) (djcb-opacity-modify t)))
(define-key my-common-keys-minor-mode-map (kbd "C-M-o")
  (lambda () (interactive) (djcb-opacity-modify)))
(define-key my-common-keys-minor-mode-map (kbd "C-M-p")
  (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;;; Installing minor mode for keys
(define-minor-mode my-common-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
  nil " my-common-keys" 'my-common-keys-minor-mode-map)

(define-global-minor-mode my-common-keys-minor-global-mode
    my-common-keys-minor-mode
  (lambda ()
    (when (not (memq major-mode
                   (list 'term-mode)))
      (my-common-keys-minor-mode))))

(my-common-keys-minor-global-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-common-keys-minor-mode))
      (let ((mykeys (assq 'my-common-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-common-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)
