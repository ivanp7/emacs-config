(defvar my-lisp-mode-keymap (make-keymap)
  "My lisp-mode keymap.")

(defvar my-extra-lisp-mode-keymap (make-keymap)
  "My extra lisp-mode keymap with SLY-conflicting keys.")

;;; SLIME commands
(define-key my-lisp-mode-keymap (kbd "<f1> <f1>")
  'hyperspec-lookup)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> a")
  'sly-apropos)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> z")
  'sly-apropos-all)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> p")
  'sly-apropos-package)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> ~")
  'common-lisp-hyperspec-format)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> #")
  'common-lisp-hyperspec-lookup-reader-macro)

(define-key my-lisp-mode-keymap (kbd "<f1> <f2> d")
  'sly-describe-symbol)
(define-key my-lisp-mode-keymap (kbd "<f1> <f2> f")
  'sly-describe-function)
(define-key my-lisp-mode-keymap (kbd "M-f")
  'sly-inspect)
(define-key my-lisp-mode-keymap (kbd "C-M-f")
  'sly-inspect-definition)

(define-key my-lisp-mode-keymap (kbd "<S-return>")
  'indent-new-comment-line)

(define-key my-extra-lisp-mode-keymap (kbd "<C-return>")
  'sly-eval-print-last-expression)

(define-key my-lisp-mode-keymap (kbd "C-<f1>")
  'sly-autodoc-manually)

(define-key my-lisp-mode-keymap (kbd "<C-f2>")
  'sly-mrepl-indent-and-complete-symbol)

(define-key my-lisp-mode-keymap (kbd "<f4>")
  'sly-eval-last-expression)
(define-key my-lisp-mode-keymap (kbd "C-<f4>")
  'sly-pprint-eval-last-expression)
(define-key my-lisp-mode-keymap (kbd "<f5>")
  (lambda ()
    (interactive)
    (if (use-region-p)
        (sly-eval-region (region-beginning) (region-end))
        (message "No region is selected to evaluate"))))
(define-key my-lisp-mode-keymap (kbd "C-<f5>")
  'sly-pprint-eval-region)
(define-key my-lisp-mode-keymap (kbd "<f6>")
  'sly-eval-defun)
(define-key my-lisp-mode-keymap (kbd "C-<f6>")
  'sly-re-evaluate-defvar)
(define-key my-lisp-mode-keymap (kbd "<f7>")
  'sly-eval-buffer)
(define-key my-lisp-mode-keymap (kbd "<f8>")
  'sly-macroexpand-1)
(define-key my-lisp-mode-keymap (kbd "S-<f8>")
  'sly-macroexpand-all)

(define-key my-lisp-mode-keymap (kbd "<f9>")
  'sly-interactive-eval)
(define-key my-lisp-mode-keymap (kbd "C-<f9>")
  'sly-edit-value)

;;; Quick editing
(defun delete-selection ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))))

(defun surround-selection (prefix-str postfix-str &optional offset-on-selection
                                                    reindent)
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
            (indent-region begin-pos (+ end-pos (length prefix-str)
                                        (length postfix-str)))))
      (let ((pos (point)))
        (insert prefix-str postfix-str)
        (if offset-on-selection
            (goto-char (+ pos offset-on-selection))
            (left-char (length postfix-str)))
        (if reindent
            (indent-region pos (+ pos (length prefix-str)
                                  (length postfix-str)))))))

(define-key my-lisp-mode-keymap (kbd "S-SPC")
  (lambda () (interactive) (delete-selection) (insert " ") (left-char 1)))

;; (define-key my-lisp-mode-keymap (kbd "<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(")))
;; (define-key my-lisp-mode-keymap (kbd "C-<f9>")
;;   (lambda () (interactive) (delete-selection) (insert "(") (left-char 1)))
;; (define-key my-lisp-mode-keymap (kbd "<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")")))
;; (define-key my-lisp-mode-keymap (kbd "C-<f10>")
;;   (lambda () (interactive) (delete-selection) (insert ")") (left-char 1)))

(define-key my-lisp-mode-keymap (kbd "M-z")
  (lambda () (interactive) (delete-selection) (insert "nil")))

(defmacro define-expansion (key prefix postfix &optional offset-on-selection
                                                 reindent
                                                 final-command
                                                 add-to-prefix-if-selection)
  `(define-key my-lisp-mode-keymap (kbd ,key)
     (lambda () (interactive)
       (surround-selection
        (if (use-region-p)
            ,(concat prefix (if add-to-prefix-if-selection
                                add-to-prefix-if-selection
                                ""))
            ,prefix)
        ,postfix ,offset-on-selection ,reindent)
       ,final-command)))

(define-expansion "C-*" "*" "*")
(define-expansion "C-+" "+" "+")
(define-expansion "C-\"" "\"" "\"")
(define-expansion "C-|" "|" "|")
(define-expansion "C-#" "#|" "|#")

(lexical-let ((make-parens (lambda ()
                             (interactive)
                             (surround-selection "(" ")" 1))))
             ;; will work in all Lisp modes
             (define-key lisp-mode-shared-map (kbd "<tab>") make-parens)
             (add-hook 'sly-mode-hook
                       (lambda ()
                         (local-set-key (kbd "<tab>") make-parens)
                         (local-set-key (kbd "M-q") make-parens)))
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
(define-expansion "M-a t" "(last " ")") ; las(t)

(define-expansion "M-a l" "(list " ")")
(define-expansion "M-a v" "(vector " ")")

(define-expansion "M-a f" "(funcall " ")")
(define-expansion "M-a y" "(apply " ")") ; appl(y)
(define-expansion "M-a e" "(eval " ")")

(define-expansion "M-a p" "(progn" ")" nil t nil "\n")

(define-expansion "M-a i" "(if " ")")
(define-expansion "M-a h" "(when " ")") ; w(h)en
(define-expansion "M-a u" "(unless " ")")

(define-expansion "M-a g a" "(and " ")")
(define-expansion "M-a g o" "(or " ")")
(define-expansion "M-a g n" "(not " ")")

(define-expansion "M-a s" "(setf " ")")
(define-expansion "M-a x" "(psetf " ")")

;; Templates
(define-expansion "M-a a" "(lambda () " ")" 7 t)
(define-expansion "M-a A" "#'(lambda () " ")" 9 t) ; l(a)mbda

(define-expansion "M-a d f" "(defun ~ ()" ")" 8 t
  (backward-delete-char-untabify 1) "\n")
(define-expansion "M-a d m" "(defmacro ~ ()" ")" 11 t
  (backward-delete-char-untabify 1) "\n")
(define-expansion "M-a d p" "(defparameter " ")" 14)
(define-expansion "M-a d v" "(defvar " ")" 8)
(define-expansion "M-a d c" "(defconstant " ")" 13)
(define-expansion "M-a d s" "(defclass ~ ()\n  ())" "" 11 t
  (backward-delete-char-untabify 1) "\n") ; clas(s)
(define-expansion "M-a d d" "(defmethod ~ ()" ")" 12 t
  (backward-delete-char-untabify 1) "\n") ; metho(d)
(define-expansion "M-a d g" "(defgeneric ~ ()\n  (:documentation \"\"))" "" 13 t
  (backward-delete-char-untabify 1) "\n")

(define-expansion "M-a b l" "(let ()" ")" 6 t nil "\n") ; (l)et
(define-expansion "M-a b o" "(let* ()" ")" 7 t nil "\n") ; c(o)nsecutive let
(define-expansion "M-a b f" "(flet (( ()))" ")" 8 t nil "\n") ; (f)unction let
(define-expansion "M-a b r" "(labels (( ()))" ")" 10 t nil "\n") ; (r)ecursive flet
(define-expansion "M-a b m" "(macrolet (( ()))" ")" 12 t nil "\n") ; (m)acro let

;;; Movement, selection, editing and other useful keybindings
;; (define-key my-lisp-mode-keymap (kbd "M-[")
;;   (lambda () (interactive) (if mark-active (deactivate-mark)) (backward-word)))
;; (define-key my-lisp-mode-keymap (kbd "M-{")
;;   (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (backward-word)))
;; (define-key my-lisp-mode-keymap (kbd "M-]")
;;   (lambda () (interactive) (if mark-active (deactivate-mark)) (forward-word)))
;; (define-key my-lisp-mode-keymap (kbd "M-}")
;;   (lambda () (interactive) (if (not mark-active) (cua-set-mark)) (forward-word)))

(define-key my-lisp-mode-keymap (kbd "M-c")
  (lambda ()
    (interactive)
    (recenter-top-bottom)
    (refresh-window)))



(define-key my-lisp-mode-keymap (kbd "C-<left>") 'backward-sexp)
(define-key my-lisp-mode-keymap (kbd "C-<right>") 'forward-sexp)
(define-key my-lisp-mode-keymap (kbd "C-<up>") 'backward-up-list)
(define-key my-lisp-mode-keymap (kbd "C-<down>") 'down-list)

(defun beginning-of-list ()
  (interactive "^") (backward-up-list) (right-char))

(defun end-of-list ()
  (interactive "^") (up-list) (left-char))

(define-key my-lisp-mode-keymap (kbd "C-<home>") 'beginning-of-list)
(define-key my-lisp-mode-keymap (kbd "C-<end>") 'end-of-list)

(defun delete-sexp-backward (&optional ARG)
  (interactive "p")
  (delete-selection)
  (deactivate-mark) (cua-set-mark) (backward-sexp ARG) (delete-selection))

(defun delete-sexp-forward (&optional ARG)
  (interactive "p")
  (delete-selection)
  (deactivate-mark) (cua-set-mark) (forward-sexp ARG) (delete-selection))

(define-key my-lisp-mode-keymap (kbd "C-<backspace>")
  'delete-sexp-backward)
(define-key my-lisp-mode-keymap (kbd "C-<delete>")
  'delete-sexp-forward)

(defvar sexp-editing-mode nil)

(define-key my-lisp-mode-keymap (kbd (concat "C-" menu-key-name))
  (lambda () (interactive)
    (if (not sexp-editing-mode)
        (progn
          (define-key my-lisp-mode-keymap
            [remap left-char] 'backward-sexp)
          (define-key my-lisp-mode-keymap
            [remap right-char] 'forward-sexp)
          (define-key my-lisp-mode-keymap
            [remap previous-line] 'backward-up-list)
          (define-key my-lisp-mode-keymap
            [remap next-line] 'down-list)
          (define-key my-lisp-mode-keymap
            [remap move-beginning-of-line] 'beginning-of-list)
          (define-key my-lisp-mode-keymap
            [remap move-end-of-line] 'end-of-list)
          (define-key my-lisp-mode-keymap
            [remap backward-sexp] 'left-char)
          (define-key my-lisp-mode-keymap
            [remap forward-sexp] 'right-char)
          (define-key my-lisp-mode-keymap
            [remap backward-up-list] 'previous-line)
          (define-key my-lisp-mode-keymap
            [remap down-list] 'next-line)
          (define-key my-lisp-mode-keymap
            [remap beginning-of-list] 'move-beginning-of-line)
          (define-key my-lisp-mode-keymap
            [remap end-of-list] 'move-end-of-line)
          (define-key my-lisp-mode-keymap
            [remap backward-delete-char-untabify] 'delete-sexp-backward)
          (define-key my-lisp-mode-keymap
            [remap delete-forward-char] 'delete-sexp-forward)
          (define-key my-lisp-mode-keymap
            [remap delete-sexp-backward] 'backward-delete-char-untabify)
          (define-key my-lisp-mode-keymap
            [remap delete-sexp-forward] 'delete-forward-char))
        (progn
          (define-key my-lisp-mode-keymap
            [remap left-char] nil)
          (define-key my-lisp-mode-keymap
            [remap right-char] nil)
          (define-key my-lisp-mode-keymap
            [remap previous-line] nil)
          (define-key my-lisp-mode-keymap
            [remap next-line] nil)
          (define-key my-lisp-mode-keymap
            [remap move-beginning-of-line] nil)
          (define-key my-lisp-mode-keymap
            [remap move-end-of-line] nil)
          (define-key my-lisp-mode-keymap
            [remap backward-sexp] nil)
          (define-key my-lisp-mode-keymap
            [remap forward-sexp] nil)
          (define-key my-lisp-mode-keymap
            [remap backward-up-list] nil)
          (define-key my-lisp-mode-keymap
            [remap down-list] nil)
          (define-key my-lisp-mode-keymap
            [remap beginning-of-list] nil)
          (define-key my-lisp-mode-keymap
            [remap end-of-list] nil)
          (define-key my-lisp-mode-keymap
            [remap backward-delete-char-untabify] nil)
          (define-key my-lisp-mode-keymap
            [remap delete-forward-char] nil)
          (define-key my-lisp-mode-keymap
            [remap delete-sexp-backward] nil)
          (define-key my-lisp-mode-keymap
            [remap delete-sexp-forward] nil)))
    (setq sexp-editing-mode (not sexp-editing-mode))
    (message "S-exp edition mode is %s"
             (if sexp-editing-mode "ON" "OFF"))))

(define-key my-extra-lisp-mode-keymap (kbd "<M-up>") 'beginning-of-defun)
(define-key my-extra-lisp-mode-keymap (kbd "<M-down>") 'end-of-defun)

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

(define-key my-lisp-mode-keymap (kbd "<S-backspace>")
  'delete-line-backward)
(define-key my-lisp-mode-keymap (kbd "<S-delete>")
  'delete-line-forward)

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

(define-key my-lisp-mode-keymap (kbd "<backtab>")
  'remove-pair-of-parens)



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
            (unless (get-text-property (line-beginning-position)
                                       'sly-repl-prompt)
              (lisp-indent-line))))))

(define-key my-lisp-mode-keymap (kbd "C-`")
  'indent-current-sexp-or-selection)

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

(define-key my-lisp-mode-keymap (kbd "C-\\") 'kill-whitespace)

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

(define-key my-lisp-mode-keymap (kbd "C-=")
  'xah-toggle-letter-case)

;;; Bugs workaround keys
(define-key my-lisp-mode-keymap (kbd "M-b")
  'comma-at-sign-add-spaces)

;;; Extensions and modes keybindings
(define-key my-lisp-mode-keymap (kbd "<f2>")
  'auto-complete)
(define-key my-lisp-mode-keymap (kbd "M-t")
  'ac-isearch)

(define-key my-lisp-mode-keymap (kbd "M-m")
  'highlight-symbol)
(define-key my-lisp-mode-keymap (kbd "M-.")
  'highlight-symbol-next)
(define-key my-lisp-mode-keymap (kbd "M-,")
  'highlight-symbol-prev)
(define-key my-lisp-mode-keymap (kbd "M-/")
  'highlight-symbol-count)
(define-key my-lisp-mode-keymap (kbd "M-n")
  'highlight-symbol-query-replace)

(defun doc-function ()
  (interactive)
  (let ((symbol-name (thing-at-point 'symbol)))
    (if (not symbol-name) (error "No symbol at point")
        (let ((symbol-name (substring-no-properties symbol-name)))
          (message "Symbol: %s" symbol-name)
          (pos-tip-show
           (format "%s" (sly-eval `(slynk:documentation-symbol
                                    ,symbol-name))))
          ;; (popup-tip
          ;;  (format "%s" (sly-eval `(slynk:documentation-symbol
          ;;                             ,symbol-name)))
          ;;  :nostrip t)
          ))))

(define-key my-lisp-mode-keymap (kbd "<C-M-mouse-1>")
  (lambda (event) (interactive "e")
    (mouse-set-point event)
    (deactivate-mark)
    (doc-function)))
(define-key my-lisp-mode-keymap (kbd "M-i")
  (lambda () (interactive)
    (deactivate-mark)
    (doc-function)))

(global-set-key (kbd "<mouse-2>")
                'hs-mouse-toggle-hiding)
;; it doesn't work as expected this way (*Completions* buffer bug):
;;(define-key my-lisp-mode-keymap (kbd "<mouse-2>")
;;  'hs-mouse-toggle-hiding)

(define-key my-lisp-mode-keymap (kbd "<M-S-mouse-4>")
  'er/expand-region)
(define-key my-lisp-mode-keymap (kbd "<M-S-mouse-5>")
  'er/contract-region)
(define-key my-lisp-mode-keymap (kbd "<mouse-3>")
  (lambda (event) (interactive "e")
    (mouse-set-point event)
    (er/mark-symbol)))
(define-key my-lisp-mode-keymap (kbd "<double-mouse-3>")
  (lambda (event) (interactive "e")
    (mouse-set-point event)
    (er/mark-symbol-with-prefix)))

(define-key my-lisp-mode-keymap (kbd "M-w")
  'highlight-stages-global-mode)

;;; Rainbow identifiers keys
(define-key my-lisp-mode-keymap (kbd "<f3>")
  'rainbow-identifiers-tune)
(define-key my-lisp-mode-keymap (kbd "<S-f3>")
  (lambda () (interactive)
    (let ((rainbow-identifiers-tune-delta -1))
      (rainbow-identifiers-tune))))
(define-key my-lisp-mode-keymap (kbd "<C-f3>")
  'rainbow-identifiers-cancel-tuning)

(define-key my-lisp-mode-keymap (kbd "M-k l")
  'rainbow-identifiers-load-tune)
(define-key my-lisp-mode-keymap (kbd "M-k s")
  'rainbow-identifiers-save-tune)

;;;; Installing minor mode for keys
(define-minor-mode my-lisp-mode-keymap-mode
  "A minor mode so that my key settings override annoying major modes."
  nil " my-lisp-keys" my-lisp-mode-keymap)

(define-minor-mode my-extra-lisp-mode-keymap-mode
  "A minor mode so that my key settings override annoying major modes."
  nil " my-lisp-keys" my-extra-lisp-mode-keymap)

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-keymap-mode)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-keymap-mode)
(add-hook 'lisp-mode-hook 'my-lisp-mode-keymap-mode)
(add-hook 'sly-mode-hook 'my-lisp-mode-keymap-mode)

(add-hook 'emacs-lisp-mode-hook 'my-extra-lisp-mode-keymap-mode)
(add-hook 'lisp-interaction-mode-hook 'my-extra-lisp-mode-keymap-mode)
(add-hook 'lisp-mode-hook 'my-extra-lisp-mode-keymap-mode)
