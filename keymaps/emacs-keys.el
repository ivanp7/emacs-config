(defvar my-emacs-keymap (make-keymap)
  "My common Emacs keymap.")

;;; Basic keys
(define-key my-emacs-keymap (kbd "C-z")
  'undo-tree-undo)
(define-key my-emacs-keymap (kbd "C-y")
  'undo-tree-redo)

(define-key my-emacs-keymap (kbd "C-S-v")
  (lambda () (interactive) (popup-menu 'yank-menu)))

(define-key my-emacs-keymap (kbd "<backspace>")
  'backward-delete-char-untabify)

;; Needed to disable destroying windows configuration by pressing <escape>
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
         ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(define-key my-emacs-keymap (kbd "<escape>")
  'keyboard-escape-quit)

;; Always open or create file with GUI
(defadvice find-file-read-args
    (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

(define-key my-emacs-keymap (kbd "C-o")
  'find-file)
(define-key my-emacs-keymap (kbd "C-p")
  'find-file-other-frame)

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

(define-key my-emacs-keymap (kbd "C-s")
  (lambda () (interactive) (save-buffer) (refresh-window)))
(define-key my-emacs-keymap (kbd "C-S-s")
  (lambda () (interactive) (write-file) (refresh-window)))
(define-key my-emacs-keymap (kbd "M-s s")
  (lambda () (interactive) (rename-current-buffer-file) (refresh-window)))

(define-key my-emacs-keymap (kbd "C-a")
  'mark-whole-buffer)
(define-key my-emacs-keymap (kbd "C-l")
  'goto-line)

(define-key my-emacs-keymap (kbd "<M-home>")
  'beginning-of-buffer)
(define-key my-emacs-keymap (kbd "<M-end>")
  'end-of-buffer)

(define-key my-emacs-keymap (kbd "C-<f1>")
  'slime-selector)

;;; Commands execution
(define-key my-emacs-keymap (kbd "C-M-x")
  'shell-command)
(define-key my-emacs-keymap (kbd "S-C-M-x")
  'repeat-complex-command)

;;;; Search
;;(define-key my-emacs-keymap (kbd "M-h")
;;  'highlight-symbol-at-point)
(define-key my-emacs-keymap (kbd "M-r")
  'highlight-regexp)
(define-key my-emacs-keymap (kbd "M-u")
  'unhighlight-regexp)

(define-key my-emacs-keymap (kbd "C-S-f")
  'occur)

;; (define-key my-emacs-keymap (kbd "C-S-G <up>")
;;   'isearch-backward)
;; (define-key my-emacs-keymap (kbd "M-C-S-G <up>")
;;   'isearch-repeat-backward)
;; (define-key my-emacs-keymap (kbd "C-S-G <down>")
;;   'isearch-forward)
;; (define-key my-emacs-keymap (kbd "M-C-S-G <down>")
;;   'isearch-repeat-forward)

;;;; Toggle input method
(define-key my-emacs-keymap (kbd "C-/")
  'toggle-input-method)

;;; Font size control keybindings
(define-key my-emacs-keymap (kbd "<C-M-next>")
  'text-scale-decrease)
(define-key my-emacs-keymap (kbd "<C-M-prior>")
  'text-scale-increase)

;; Horizontal wheeling
(define-key my-emacs-keymap (kbd "<C-M-mouse-4>")
  '(lambda ()
    (interactive)
    (scroll-right 10 t)
    (refresh-window)))
(define-key my-emacs-keymap (kbd "<C-M-mouse-5>")
  '(lambda ()
    (interactive)
    (scroll-left 10 t)
    (refresh-window)))

;;; Transparency control keybindings

;; C-M-i will decrease opacity (== increase transparency
;; C-M-o will increase opacity (== decrease transparency)
;; C-M-p will returns the state to normal
(define-key my-emacs-keymap (kbd "C-M-i")
  (lambda () (interactive) (djcb-opacity-modify t)))
(define-key my-emacs-keymap (kbd "C-M-o")
  (lambda () (interactive) (djcb-opacity-modify)))
(define-key my-emacs-keymap (kbd "C-M-p")
  (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;;; Open hotkey help file in a new frame
(define-key my-emacs-keymap (kbd "<f1> SPC")
  (lambda () (interactive)
     (find-file-other-frame (concat cl-ide-init-path
                                    "info/keymaps-description-ru.org"))))

;;;; Redraw display
(define-key my-emacs-keymap (kbd "C-`")
  'redraw-display)

;;;; Installing minor mode for keys
(define-minor-mode my-emacs-keymap-mode
    "A minor mode so that my key settings override annoying major modes."
  t " my-emacs-keys" my-emacs-keymap)

(define-global-minor-mode my-emacs-keymap-global-mode
    my-emacs-keymap-mode
  (lambda ()
    (when (not (memq major-mode (list 'term-mode)))
      (my-emacs-keymap-mode))))

(my-emacs-keymap-global-mode 1)

(defadvice load (after give-custom-keybindings-priority)
  "Try to ensure that these custom keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-emacs-keymap-mode))
      (let ((mykeys (assq 'my-emacs-keymap-mode minor-mode-map-alist)))
        (assq-delete-all 'my-emacs-keymap-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)
