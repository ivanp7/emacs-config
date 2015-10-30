(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;;; Basic keys
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key my-keys-minor-mode-map (kbd "C-y") 'undo-tree-redo)

(define-key my-keys-minor-mode-map (kbd "C-S-v")
  (lambda () (interactive) (popup-menu 'yank-menu)))

(defvar *cut-mode* nil)
(defvar *prev-region-beg* nil)
(defvar *prev-region-end* nil)

;; (defun fast-copy-paste ()
;;   (interactive)
;;   (if (use-region-p)
;;       (let ((message-log-max nil))
;;         (if (or (not *cut-mode*) (/= (region-beginning) *prev-region-beg*)
;;                (/= (region-end) *prev-region-end*))
;;             (let ((begin-pos (region-beginning)) (end-pos (region-end)) (p (point)))
;;               (cua-copy-region nil)
;;               ;; (if (= p begin-pos)
;;               ;;     (progn
;;               ;;       (goto-char end-pos)
;;               ;;       (cua-set-mark)
;;               ;;       (goto-char begin-pos))
;;               ;;     (progn
;;               ;;       (goto-char begin-pos)
;;               ;;       (cua-set-mark)
;;               ;;       (goto-char end-pos)))
;;               (activate-mark)
;;               (setf *cut-mode* t)
;;               (setf *prev-region-beg* begin-pos)
;;               (setf *prev-region-end* end-pos)
;;               (message "Copied"))
;;             (progn
;;               (cua-cut-region nil)
;;               (setf *cut-mode* nil)
;;               (setf *prev-region-beg* nil)
;;               (setf *prev-region-end* nil)
;;               (message "Cut"))))
;;       (let ((message-log-max nil))
;;         (cua-paste nil)
;;         (message "Pasted"))))

;; (define-key my-keys-minor-mode-map (kbd "M-SPC") 'fast-copy-paste)

;; Needed to disable destroying windows configuration by pressing <escape>
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
         ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)

;; Always open or create file with GUI
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))

(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)

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

(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-S-s") 'write-file)
(define-key my-keys-minor-mode-map (kbd "M-s s") 'rename-current-buffer-file)

(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "C-l") 'goto-line)

;;; Buffers and windows configuration control keys
(define-key my-keys-minor-mode-map (kbd "<C-apps>") 'kill-this-buffer)
(define-key my-keys-minor-mode-map (kbd "<S-C-apps>")
  (lambda () (interactive) (other-window 1) (kill-this-buffer) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "C-'") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-,") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-.") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<")
  (lambda () (interactive) (other-window 1) (previous-buffer) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "C->")
  (lambda () (interactive) (other-window 1) (next-buffer) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "<f2>") 'slime-selector)

(define-key my-keys-minor-mode-map (kbd "<apps> <left>") 'buf-move-left)
(define-key my-keys-minor-mode-map (kbd "<apps> <S-left>")
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-left) (setq buffer-move-behavior 'move)))
(define-key my-keys-minor-mode-map (kbd "<apps> <up>") 'buf-move-up)
(define-key my-keys-minor-mode-map (kbd "<apps> <S-up>")
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-up) (setq buffer-move-behavior 'move)))
(define-key my-keys-minor-mode-map (kbd "<apps> <right>") 'buf-move-right)
(define-key my-keys-minor-mode-map (kbd "<apps> <S-right>")
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-right) (setq buffer-move-behavior 'move)))
(define-key my-keys-minor-mode-map (kbd "<apps> <down>") 'buf-move-down)
(define-key my-keys-minor-mode-map (kbd "<apps> <S-down>")
  (lambda () (interactive)
     (setq buffer-move-behavior 'swap) (buf-move-down) (setq buffer-move-behavior 'move)))

(define-key my-keys-minor-mode-map (kbd "<apps> C-<left>") 'split-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "<apps> C-<up>") 'split-window-vertically)
(define-key my-keys-minor-mode-map (kbd "<apps> C-<right>") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "<apps> C-<down>") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "<apps> <backspace>") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "<apps> <delete>") 'delete-window)

;;; Commands execution
(define-key my-keys-minor-mode-map (kbd "C-M-x") 'shell-command)
(define-key my-keys-minor-mode-map (kbd "S-C-M-x") 'repeat-complex-command)

;;; SLIME commands
(define-key my-keys-minor-mode-map (kbd "<S-escape>") 'slime-interrupt)

(define-key my-keys-minor-mode-map (kbd "<f1> <f1>") 'slime-hyperspec-lookup)

(define-key my-keys-minor-mode-map (kbd "M-e") 'slime-interactive-eval)
(define-key my-keys-minor-mode-map (kbd "M-d") 'slime-edit-value)
(define-key my-keys-minor-mode-map (kbd "M-f") 'slime-inspect)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'slime-inspect-definition)

(define-key my-keys-minor-mode-map (kbd "<backtab>") 'slime-close-all-parens-in-sexp)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'slime-insert-balanced-comments)
(define-key my-keys-minor-mode-map (kbd "S-<f3>") 'slime-remove-balanced-comments)
(define-key my-keys-minor-mode-map (kbd "<f4>") 'slime-eval-last-expression)
(define-key my-keys-minor-mode-map (kbd "C-<f4>") 'slime-pprint-eval-last-expression)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'slime-eval-region)
(define-key my-keys-minor-mode-map (kbd "C-<f5>") 'slime-pprint-eval-region)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'slime-eval-defun)
(define-key my-keys-minor-mode-map (kbd "S-<f6>") 'slime-call-defun)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'slime-eval-buffer)
(define-key my-keys-minor-mode-map (kbd "<f8>") 'slime-macroexpand-1)
(define-key my-keys-minor-mode-map (kbd "S-<f8>") 'slime-macroexpand-all)

(defun delete-selection ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))))

(defun surround-selection (prefix-str postfix-str &optional offset-on-selection reindent)
  (interactive)
  (if (use-region-p)
      (let ((begin-pos (region-beginning)) (end-pos (region-end)))
        (deactivate-mark)
        (goto-char begin-pos)
        (insert prefix-str)
        (goto-char (+ end-pos (length prefix-str)))
        (insert postfix-str)
        (if offset-on-selection
            (goto-char (+ begin-pos offset-on-selection))
            (left-char (length postfix-str)))
        (if reindent
            (indent-region begin-pos (+ end-pos (length prefix-str) (length postfix-str)))))
      (let ((pos (point)))
        (insert prefix-str postfix-str)
        (if offset-on-selection
            (goto-char (+ pos offset-on-selection))
            (left-char (length postfix-str)))
        (if reindent
            (indent-region pos (+ pos (length prefix-str) (length postfix-str)))))))

(define-key my-keys-minor-mode-map (kbd "M-SPC")
  (lambda () (interactive) (delete-selection) (insert " ") (left-char 1)))

;; (define-key my-keys-minor-mode-map (kbd "<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(")))
;; (define-key my-keys-minor-mode-map (kbd "C-<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(") (left-char 1)))
;; (define-key my-keys-minor-mode-map (kbd "<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")")))
;; (define-key my-keys-minor-mode-map (kbd "C-<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")") (left-char 1)))

(define-key my-keys-minor-mode-map (kbd "M-z")
  (lambda () (interactive) (delete-selection) (insert "nil")))

(defmacro define-expansion (key prefix postfix &optional offset-on-selection reindent)
  `(define-key my-keys-minor-mode-map (kbd ,key)
     (lambda () (interactive) (surround-selection ,prefix ,postfix ,offset-on-selection ,reindent))))

(define-expansion "C-*" "*" "*")
(define-expansion "C-+" "+" "+")
(define-expansion "C-\"" "\"" "\"")
(define-expansion "C-|" "|" "|")
(define-expansion "C-#" "#|" "|#")

(let ((make-parens (lambda () (interactive) (surround-selection "(" ")" 1))))
  (define-key slime-repl-mode-map (kbd "TAB") make-parens)
  (define-key lisp-mode-map (kbd "TAB") make-parens)
  (define-key emacs-lisp-mode-map (kbd "TAB") make-parens))

(define-expansion "M-a '" "'(" ")" 2)
(define-expansion "M-a `" "`(" ")" 2)
(define-expansion "M-a ," ",(" ")" 2)
(define-expansion "M-a @" ",@(" ")" 3)

(define-expansion "M-a #" "#(" ")")

(define-expansion "M-a o" "(cons " ")") ; c(o)ns
(define-expansion "M-a q" "(car " ")")
(define-expansion "M-a w" "(cdr " ")")

(define-expansion "M-a 1" "(first " ")")
(define-expansion "M-a 2" "(second " ")")
(define-expansion "M-a 3" "(third " ")")
(define-expansion "M-a 4" "(fourth " ")")
(define-expansion "M-a 5" "(fifth " ")")
(define-expansion "M-a 6" "(sixth " ")")
(define-expansion "M-a 7" "(seventh " ")")
(define-expansion "M-a 8" "(eighth " ")")
(define-expansion "M-a 9" "(ninth " ")")
(define-expansion "M-a 0" "(tenth " ")")
(define-expansion "M-a n" "(nth " ")")
(define-expansion "M-a r" "(rest " ")")
(define-expansion "M-a t" "(last " ")")

(define-expansion "M-a l" "(list " ")")
(define-expansion "M-a v" "(vector " ")")

(define-expansion "M-a f" "(funcall " ")")
(define-expansion "M-a y" "(apply " ")") ; appl(y)
(define-expansion "M-a e" "(eval " ")")

(define-expansion "M-a p" "(progn\n" ")" nil t)

(define-expansion "M-a u" "(null " ")")

(define-expansion "M-a s" "(setf " ")")

;; Templates
(define-expansion "M-a a" "#'(lambda () " ")" 11 t) ; l(a)mbda

(define-expansion "M-a d f" "(defun ~ ()\n" ")" 8 t)
(define-expansion "M-a d m" "(defmacro ~ ()\n" ")" 11 t)
(define-expansion "M-a d p" "(defparameter " ")")
(define-expansion "M-a d v" "(defvar " ")")
(define-expansion "M-a d c" "(defconstant " ")")
(define-expansion "M-a d s" "(defclass ~ ()\n  (" "))" 11 t) ; clas(s)
(define-expansion "M-a d d" "(defmethod ~ ()\n" ")" 12 t) ; metho(d)
(define-expansion "M-a d g" "(defgeneric ~ ()\n  (:documentation \"" "\"))" 13 t)

(define-expansion "M-a b l" "(let (())\n" ")" 7 t) ; (l)et
(define-expansion "M-a b o" "(let* (())\n" ")" 8 t)
(define-expansion "M-a b f" "(flet (())\n" ")" 8 t) ; (f)unction
(define-expansion "M-a b r" "(labels (())\n" ")" 10 t) ; (r)ecursion allowed

;;; Movement, selection, editing and other useful keybindings
(define-key my-keys-minor-mode-map (kbd "<C-left>") 'backward-sexp)
(define-key my-keys-minor-mode-map (kbd "<C-right>") 'forward-sexp)

(define-key my-keys-minor-mode-map (kbd "M-[")
  (lambda () (interactive) (if mark-active (deactivate-mark)) (backward-word)))
(define-key my-keys-minor-mode-map (kbd "M-{")
  (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (backward-word)))
(define-key my-keys-minor-mode-map (kbd "M-]")
  (lambda () (interactive) (if mark-active (deactivate-mark)) (forward-word)))
(define-key my-keys-minor-mode-map (kbd "M-}")
  (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (forward-word)))

(define-key my-keys-minor-mode-map (kbd "M-c") 'recenter-top-bottom)

(defun delete-line-forward ()
  (interactive)
  (deactivate-mark)
  (delete-region (point)
                 (progn (move-end-of-line 1) (point))))

(defun delete-line-backward ()
  (interactive)
  (deactivate-mark)
  (delete-region (point)
                 (progn (move-beginning-of-line 1) (point))))

(define-key my-keys-minor-mode-map (kbd "<S-backspace>") 'delete-line-backward)
(define-key my-keys-minor-mode-map (kbd "<S-delete>") 'delete-line-forward)

(define-key my-keys-minor-mode-map (kbd "<C-backspace>")
  (lambda () (interactive) (deactivate-mark) (cua-set-mark) (backward-sexp) (delete-selection)))
(define-key my-keys-minor-mode-map (kbd "<C-delete>")
  (lambda () (interactive) (deactivate-mark) (cua-set-mark) (forward-sexp) (delete-selection)))

(defun remove-pair-of-parens ()
  (interactive)
  (let ((left (condition-case nil
                  (save-excursion
                    (backward-up-list)
                    (point))
                (error nil)))
        (right (condition-case nil
                   (save-excursion
                     (up-list)
                     (point))
                 (error nil)))
        (pos (point)))
    (if (and left right)
        (progn
          (goto-char right)
          (delete-backward-char 1)
          (goto-char left)
          (delete-char 1)
          (goto-char (- pos 1))))))

(define-key my-keys-minor-mode-map (kbd "<M-delete>") 'remove-pair-of-parens)

(defun xah-toggle-letter-case (fp1 fp2)
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

In lisp code, fp1 fp2 are region boundary.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-04-09"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
       (let ((bds (bounds-of-thing-at-point 'word)))
         (list (car bds) (cdr bds)))))
  (let ((deactivate-mark nil))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
      ((equal 0 (get this-command 'state))
       (upcase-initials-region fp1 fp2)
       (put this-command 'state 1))
      ((equal 1  (get this-command 'state))
       (upcase-region fp1 fp2)
       (put this-command 'state 2))
      ((equal 2 (get this-command 'state))
       (downcase-region fp1 fp2)
       (put this-command 'state 0)))))

(define-key my-keys-minor-mode-map (kbd "C-1") 'xah-toggle-letter-case)

;;(define-key my-keys-minor-mode-map (kbd "M-h") 'highlight-symbol-at-point)
(define-key my-keys-minor-mode-map (kbd "M-r") 'highlight-regexp)
(define-key my-keys-minor-mode-map (kbd "M-u") 'unhighlight-regexp)

(define-key my-keys-minor-mode-map (kbd "C-S-f") 'occur)

;;; Bugs workaround keys
(define-key my-keys-minor-mode-map (kbd "M-b") 'comma-at-sign-add-spaces)

;;; Extensions and modes keybindings
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'auto-complete)

(define-key my-keys-minor-mode-map (kbd "M-h") 'highlight-symbol)
(define-key my-keys-minor-mode-map (kbd "M-.") 'highlight-symbol-next)
(define-key my-keys-minor-mode-map (kbd "M-,") 'highlight-symbol-prev)
(define-key my-keys-minor-mode-map (kbd "M-m") 'highlight-symbol-count)
(define-key my-keys-minor-mode-map (kbd "M-y") 'highlight-symbol-query-replace)

(defun doc-function ()
  (interactive)
  (let ((symbol-name (thing-at-point 'symbol)))
    (if (not symbol-name) (error "No symbol at point")
        (let ((symbol-name (substring-no-properties symbol-name)))
          (message "Symbol: %s" symbol-name)
          (popup-tip (format "%s" (slime-eval `(swank:documentation-symbol ,symbol-name)))
                     :nostrip t)))))

(define-key my-keys-minor-mode-map (kbd "<C-M-mouse-1>")
  (lambda (event) (interactive "e") (mouse-set-point event) (deactivate-mark) (doc-function)))
(define-key my-keys-minor-mode-map (kbd "M-i")
  (lambda () (interactive) (deactivate-mark) (doc-function)))

(global-set-key (kbd "<mouse-2>") 'hs-mouse-toggle-hiding)
;; it doesn't work as expected this way (*Completions* buffer bug):
;;(define-key my-keys-minor-mode-map (kbd "<mouse-2>") 'hs-mouse-toggle-hiding)

(define-key my-keys-minor-mode-map (kbd "M-/") 'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "<mouse-3>")
  (lambda (event) (interactive "e") (mouse-set-point event) (er/mark-symbol)))
(define-key my-keys-minor-mode-map (kbd "<double-mouse-3>")
  (lambda (event) (interactive "e") (mouse-set-point event) (er/mark-symbol-with-prefix)))

(define-key my-keys-minor-mode-map (kbd "M-q") 'highlight-stages-global-mode)

;;; Rainbow identifiers keys
(define-key my-keys-minor-mode-map (kbd "<pause>") 'rainbow-identifiers-tune)
(define-key my-keys-minor-mode-map (kbd "<S-pause>")
  (lambda () (interactive) (let ((rainbow-identifiers-tune-delta -1)) (rainbow-identifiers-tune))))
(define-key my-keys-minor-mode-map (kbd "<M-pause>") 'rainbow-identifiers-cancel-tuning)
(define-key my-keys-minor-mode-map (kbd "M-j l") 'rainbow-identifiers-load-tune)
(define-key my-keys-minor-mode-map (kbd "M-j s") 'rainbow-identifiers-save-tune)

;;; Font size control keybindings
(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=") 'text-scale-increase)

;;; Transparency control keybindings

;; C-8 will decrease opacity (== increase transparency
;; C-9 will increase opacity (== decrease transparency)
;; C-0 will returns the state to normal
(define-key my-keys-minor-mode-map (kbd "C-8")
  (lambda () (interactive) (djcb-opacity-modify t)))
(define-key my-keys-minor-mode-map (kbd "C-9")
  (lambda () (interactive) (djcb-opacity-modify)))
(define-key my-keys-minor-mode-map (kbd "C-0")
  (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;;; Installing minor mode for keys
(define-minor-mode my-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)
