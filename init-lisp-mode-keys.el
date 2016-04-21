(defvar my-lisp-keys-minor-mode-map (make-keymap) "my-common-keys-minor-mode keymap.")

;;; SLIME commands
(define-key my-lisp-keys-minor-mode-map (kbd "<f1> <f1>") 'slime-hyperspec-lookup)

(define-key my-lisp-keys-minor-mode-map (kbd "<S-escape>") 'slime-interrupt)
(define-key my-lisp-keys-minor-mode-map (kbd "M-e") 'slime-interactive-eval)
(define-key my-lisp-keys-minor-mode-map (kbd "M-d") 'slime-edit-value)
(define-key my-lisp-keys-minor-mode-map (kbd "M-f") 'slime-inspect)
(define-key my-lisp-keys-minor-mode-map (kbd "C-M-f") 'slime-inspect-definition)

(define-key my-lisp-keys-minor-mode-map (kbd "<C-insert>") 'slime-close-all-parens-in-sexp)
(define-key my-lisp-keys-minor-mode-map (kbd "<S-return>") 'indent-new-comment-line)

(define-key my-lisp-keys-minor-mode-map (kbd "C-;") 'slime-insert-balanced-comments)
(define-key my-lisp-keys-minor-mode-map (kbd "C-:") 'slime-remove-balanced-comments)

(define-key my-lisp-keys-minor-mode-map (kbd "<f4>") 'slime-eval-last-expression)
(define-key my-lisp-keys-minor-mode-map (kbd "C-<f4>") 'slime-pprint-eval-last-expression)
(define-key my-lisp-keys-minor-mode-map (kbd "<f5>")
  (lambda ()
    (interactive)
    (if (use-region-p)
        (slime-eval-region (region-beginning) (region-end))
        (message "No region is selected to evaluate"))))
(define-key my-lisp-keys-minor-mode-map (kbd "C-<f5>") 'slime-pprint-eval-region)
(define-key my-lisp-keys-minor-mode-map (kbd "<f6>") 'slime-eval-defun)
(define-key my-lisp-keys-minor-mode-map (kbd "C-<f6>") 'slime-re-evaluate-defvar)
(define-key my-lisp-keys-minor-mode-map (kbd "S-<f6>") 'slime-call-defun)
(define-key my-lisp-keys-minor-mode-map (kbd "<f7>") 'slime-eval-buffer)
(define-key my-lisp-keys-minor-mode-map (kbd "<f8>") 'slime-macroexpand-1)
(define-key my-lisp-keys-minor-mode-map (kbd "S-<f8>") 'slime-macroexpand-all)
(define-key my-lisp-keys-minor-mode-map (kbd "<f9>") 'slime-complete-form)

;;; Quick editing
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

;; (define-key my-lisp-keys-minor-mode-map (kbd "M-SPC")
;;   (lambda () (interactive) (delete-selection) (insert " ") (left-char 1)))

;; (define-key my-lisp-keys-minor-mode-map (kbd "<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(")))
;; (define-key my-lisp-keys-minor-mode-map (kbd "C-<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(") (left-char 1)))
;; (define-key my-lisp-keys-minor-mode-map (kbd "<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")")))
;; (define-key my-lisp-keys-minor-mode-map (kbd "C-<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")") (left-char 1)))

(define-key my-lisp-keys-minor-mode-map (kbd "M-z")
  (lambda () (interactive) (delete-selection) (insert "nil")))

(defmacro define-expansion (key prefix postfix &optional offset-on-selection reindent)
  `(define-key my-lisp-keys-minor-mode-map (kbd ,key)
     (lambda () (interactive) (surround-selection ,prefix ,postfix ,offset-on-selection ,reindent))))

(define-expansion "C-*" "*" "*")
(define-expansion "C-+" "+" "+")
(define-expansion "C-\"" "\"" "\"")
(define-expansion "C-|" "|" "|")
(define-expansion "C-#" "#|" "|#")

(lexical-let ((make-parens (lambda () (interactive) (surround-selection "(" ")" 1))))
  (define-key lisp-mode-shared-map (kbd "<tab>") make-parens) ; will work in all Lisp modes
  (define-key lisp-mode-shared-map (kbd "M-q") make-parens)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "M-q") make-parens))))

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
(define-expansion "M-a b m" "(macrolet (())\n" ")" 12 t) ; (m)acro

;;; Movement, selection, editing and other useful keybindings
(define-key my-lisp-keys-minor-mode-map (kbd "M-[")
  (lambda () (interactive) (if mark-active (deactivate-mark)) (backward-word)))
(define-key my-lisp-keys-minor-mode-map (kbd "M-{")
  (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (backward-word)))
(define-key my-lisp-keys-minor-mode-map (kbd "M-]")
  (lambda () (interactive) (if mark-active (deactivate-mark)) (forward-word)))
(define-key my-lisp-keys-minor-mode-map (kbd "M-}")
  (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (forward-word)))

(define-key my-lisp-keys-minor-mode-map (kbd "M-c") 'recenter-top-bottom)



(defvar sexp-edition-mode nil)

(setq mark-pos nil)

(define-key my-lisp-keys-minor-mode-map (kbd (concat "C-" menu-key-name))
  (lambda () (interactive)
          (setq sexp-edition-mode (not sexp-edition-mode))
          (message "S-exp edition mode is %s" (if sexp-edition-mode "ON" "OFF"))))

(defmacro define-movement-key (key prefix-command conseq-command alter-command)
  `(define-key my-lisp-keys-minor-mode-map (kbd ,key)
     (lambda () (interactive)
             ,prefix-command
             (if (null sexp-edition-mode)
                 ,conseq-command
                 ,alter-command))))

(defmacro define-movement (key prefix-command conseq-command alter-command)
  `(progn
     (define-movement-key ,key ,prefix-command ,conseq-command ,alter-command)
     (define-movement-key (concat "C-" ,key) ,prefix-command ,alter-command ,conseq-command)))

(define-movement "<left>"
    (deactivate-mark) (left-char) (backward-sexp))
(define-movement "S-<left>"
    (unless mark-active (cua-set-mark)) (left-char) (backward-sexp))

(define-movement "<right>"
    (deactivate-mark) (right-char) (forward-sexp))
(define-movement "S-<right>"
    (unless mark-active (cua-set-mark)) (right-char) (forward-sexp))

(define-movement "<up>"
    (deactivate-mark) (previous-line) (backward-up-list))
(define-movement "S-<up>"
    (unless mark-active (cua-set-mark)) (previous-line) (backward-up-list))

(define-movement "<down>"
    (deactivate-mark) (next-line) (down-list))
(define-movement "S-<down>"
    (unless mark-active (cua-set-mark)) (next-line) (down-list))

;; following is defined in init-extensions.el
;; (define-key my-lisp-keys-minor-mode-map (kbd "<M-up>") 'beginning-of-defun)
;; (define-key my-lisp-keys-minor-mode-map (kbd "<M-down>") 'end-of-defun)



(define-movement "<backspace>" nil
  (if (use-region-p) (delete-selection) (backward-delete-char-untabify 1))
  (progn (deactivate-mark) (cua-set-mark) (backward-sexp) (delete-selection)))
(define-movement "<delete>" nil
  (if (use-region-p) (delete-selection) (delete-forward-char 1))
  (progn (deactivate-mark) (cua-set-mark) (forward-sexp) (delete-selection)))

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

(define-key my-lisp-keys-minor-mode-map (kbd "<S-backspace>") 'delete-line-backward)
(define-key my-lisp-keys-minor-mode-map (kbd "<S-delete>") 'delete-line-forward)

(defun remove-pair-of-parens ()
  (interactive)
  (let ((left (condition-case nil (save-excursion
                                    (backward-up-list)
                                    (point))
                (error nil)))
        (right (condition-case nil (save-excursion
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

(define-key my-lisp-keys-minor-mode-map (kbd "<backtab>") 'remove-pair-of-parens)



(defun indent-current-sexp-or-selection ()
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
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
            (indent-region left right)
            (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
              (lisp-indent-line))))))

(define-key my-lisp-keys-minor-mode-map (kbd "<C-tab>") 'indent-current-sexp-or-selection)

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (let ((pos (point)))
    (save-excursion
      (save-restriction
        (save-match-data
          (progn
            (re-search-backward "[^ \t\r\n]" nil t)
            (re-search-forward "[ \t\r\n]+" nil t)
            (let ((match-beg (match-beginning 0)) space-inserted)
              (when (<= match-beg pos)
                (replace-match "" nil nil)
                (when (not (or (eql (char-after) ?\))
                               (eql (char-before) ?\()))
                  (insert " ")
                  (setq space-inserted t))
                (setq pos (if (and (< match-beg pos) space-inserted)
                              (1+ match-beg) match-beg))))))))
    (goto-char pos)))

(define-key my-lisp-keys-minor-mode-map (kbd "C-\\") 'kill-whitespace)

;;; Other functions

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

(define-key my-lisp-keys-minor-mode-map (kbd "C-=") 'xah-toggle-letter-case)

;;; Bugs workaround keys
(define-key my-lisp-keys-minor-mode-map (kbd "M-b") 'comma-at-sign-add-spaces)

;;; Extensions and modes keybindings
(define-key my-lisp-keys-minor-mode-map (kbd "C-`") 'auto-complete)
(define-key my-lisp-keys-minor-mode-map (kbd "M-t") 'ac-isearch)

(define-key my-lisp-keys-minor-mode-map (kbd "M-m") 'highlight-symbol)
(define-key my-lisp-keys-minor-mode-map (kbd "M-.") 'highlight-symbol-next)
(define-key my-lisp-keys-minor-mode-map (kbd "M-,") 'highlight-symbol-prev)
(define-key my-lisp-keys-minor-mode-map (kbd "M-/") 'highlight-symbol-count)
(define-key my-lisp-keys-minor-mode-map (kbd "M-n") 'highlight-symbol-query-replace)

(defun doc-function ()
  (interactive)
  (let ((symbol-name (thing-at-point 'symbol)))
    (if (not symbol-name) (error "No symbol at point")
        (let ((symbol-name (substring-no-properties symbol-name)))
          (message "Symbol: %s" symbol-name)
          (popup-tip (format "%s" (slime-eval `(swank:documentation-symbol ,symbol-name)))
                     :nostrip t)))))

(define-key my-lisp-keys-minor-mode-map (kbd "<C-M-mouse-1>")
  (lambda (event) (interactive "e") (mouse-set-point event) (deactivate-mark) (doc-function)))
(define-key my-lisp-keys-minor-mode-map (kbd "M-i")
  (lambda () (interactive) (deactivate-mark) (doc-function)))

(global-set-key (kbd "<mouse-2>") 'hs-mouse-toggle-hiding)
;; it doesn't work as expected this way (*Completions* buffer bug):
;;(define-key my-lisp-keys-minor-mode-map (kbd "<mouse-2>") 'hs-mouse-toggle-hiding)

(define-key my-lisp-keys-minor-mode-map (kbd "<M-mouse-4>") 'er/expand-region)
(define-key my-lisp-keys-minor-mode-map (kbd "<M-mouse-5>") 'er/contract-region)
(define-key my-lisp-keys-minor-mode-map (kbd "<mouse-3>")
  (lambda (event) (interactive "e") (mouse-set-point event) (er/mark-symbol)))
(define-key my-lisp-keys-minor-mode-map (kbd "<double-mouse-3>")
  (lambda (event) (interactive "e") (mouse-set-point event) (er/mark-symbol-with-prefix)))

(define-key my-lisp-keys-minor-mode-map (kbd "M-w") 'highlight-stages-global-mode)

;;; Rainbow identifiers keys
(define-key my-lisp-keys-minor-mode-map (kbd "M-k") 'rainbow-identifiers-tune)
(define-key my-lisp-keys-minor-mode-map (kbd "M-K")
  (lambda () (interactive) (let ((rainbow-identifiers-tune-delta -1)) (rainbow-identifiers-tune))))
(define-key my-lisp-keys-minor-mode-map (kbd "M-j r") 'rainbow-identifiers-cancel-tuning)
(define-key my-lisp-keys-minor-mode-map (kbd "M-j l") 'rainbow-identifiers-load-tune)
(define-key my-lisp-keys-minor-mode-map (kbd "M-j s") 'rainbow-identifiers-save-tune)

;;;; Installing minor mode for keys
(define-minor-mode my-lisp-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
  nil " my-lisp-keys" 'my-lisp-keys-minor-mode-map)

(add-hook 'emacs-lisp-mode-hook 'my-lisp-keys-minor-mode)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-keys-minor-mode)
(add-hook 'lisp-mode-hook 'my-lisp-keys-minor-mode)
(add-hook 'slime-repl-mode-hook 'my-lisp-keys-minor-mode)
