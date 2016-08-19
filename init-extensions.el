;;;; Org-mode
(require 'org)

(setq org-directory (default-value 'default-directory))
(setq org-default-notes-file (concat org-directory "ivanp7.org"))
(setq org-agenda-files (list (concat org-directory "ivanp7.org")))
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-startup-indented t)
(setq org-M-RET-may-split-line nil)
(setq org-startup-truncated nil)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
;;(setq org-startup-folded nil)
(setq org-footnote-auto-adjust t)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-`") 'org-todo)
            (define-key org-mode-map (kbd "<mouse-2>")
              (lambda (event) (interactive "e") (mouse-set-point event) (org-cycle)))
            (define-key org-mode-map (kbd "<S-mouse-2>")
              (lambda (event) (interactive "e") (mouse-set-point event) (org-shifttab)))))

(setq org-todo-keywords '((sequence "TODO" "DELAYED" "INPROGRESS" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DELAYED" . (:foreground "peru" :weight bold))
        ("INPROGRESS" . (:foreground "OrangeRed1" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "gray" :weight bold))))

(setq org-capture-templates
      '(("j" "Journal" entry (file+datetree "ivanp7-journal.org")
         "* %?\nEntered on %U\n")))

;; Lisp support
(require 'ob-lisp)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;; LaTeX

(add-to-list 'org-latex-packages-alist '("russian" "babel"))
(add-to-list 'org-latex-packages-alist '("unicode" "hyperref"))

;;;; NLinum mode

;; Permanently disable linum-mode to avoid conflicts with nlinum-mode
(define-minor-mode linum-mode
    ""
  :lighter ""
  (setq linum-mode nil))

(global-nlinum-mode)

;; Select entire line by clicking at line number
(defvar *linum-mdown-line* nil)

(defun md-select-linum (event)
  (interactive "e")
  (mouse-set-point event)
  (goto-line (nlinum--line-number-at-pos))
  (set-mark (point))
  (setq *linum-mdown-line* (nlinum--line-number-at-pos)))

(defun mu-select-linum (event)
  (interactive "e")
  (when *linum-mdown-line*
    (let (mu-line)
      (mouse-set-point event)
      (setq mu-line (nlinum--line-number-at-pos))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown-line* nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; Preset width nlinum
(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum--width
                  (length (number-to-string
                           (count-lines (point-min) (point-max)))))))

;; Fix 'nlinum--face-width: Invalid face: linum' error
(defvar frame-ready nil)
(add-hook 'after-make-frame-functions (lambda (frame) (set-frame-parameter frame 'frame-ready t)))
(add-hook 'after-init-hook (lambda () (set-frame-parameter nil 'frame-ready t)))

(defun nlinum--setup-window ()
  (let ((width (if (and frame-ready (display-graphic-p)) ;; <-- Here
                   (ceiling
                    (let ((width (nlinum--face-width 'linum)))
                      (if width
                          (/ (* nlinum--width 1.0 width)
                             (frame-char-width))
                          (/ (* nlinum--width 1.0
                                (nlinum--face-height 'linum))
                             (frame-char-height)))))
                   nlinum--width)))
    (set-window-margins nil (if nlinum-mode width)
                        (cdr (window-margins)))))

;;;; SLIME
(require 'slime-autoloads)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq slime-contribs '(slime-fancy slime-repl-ansi-color))
(setq slime-repl-ansi-color t)

(setq slime-net-coding-system 'utf-8-unix)

(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (ecl ("ecl"))))
;; Optionally, specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)

;; Common Lisp HyperSpec
(setq common-lisp-hyperspec-root (expand-file-name "../info/HyperSpec/HyperSpec/"))

(setq lisp-indent-function 'common-lisp-indent-function)

;;; Improve usability of slime-apropos: slime-apropos-minor-mode
(defvar slime-apropos-anchor-regexp "^[^ ]")

(defun slime-apropos-next-anchor ()
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (if (re-search-forward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
        (goto-char pt)
        (error "anchor not found"))))

(defun slime-apropos-prev-anchor ()
  (interactive)
  (let ((p (point)))
    (if (re-search-backward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
        (goto-char p)
        (error "anchor not found"))))

(defvar slime-apropos-minor-mode-map (make-sparse-keymap))

(define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
(define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)

(define-minor-mode slime-apropos-minor-mode "")

(defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
  ""
  (when (get-buffer "*slime-apropos*")
    (with-current-buffer "*slime-apropos*" (slime-apropos-minor-mode 1))))

;;; SLIME starup time initialization
(defun slime-startup-time-init ()
  nil)

;;; SLIME faces
(defun configure-slime-faces ()
  (set-face-attribute 'slime-repl-input-face nil
                      :foreground "light goldenrod" :slant 'normal :weight 'normal)
  (set-face-attribute 'slime-repl-output-face nil ; inherits string face
                      :foreground "steel blue" :slant 'italic :weight 'normal)
  (set-face-attribute 'slime-repl-inputed-output-face nil
                      :foreground "firebrick" :slant 'italic :weight 'normal)
  (set-face-attribute 'slime-repl-output-mouseover-face nil
                      :box "chocolate")
  (set-face-attribute 'slime-repl-result-face nil
                      :foreground "chocolate" :slant 'italic)
  (set-face-attribute 'slime-repl-prompt-face nil ; inherits keyword face
                      :foreground "sea green" :slant 'normal :weight 'bold)

  (set-face-attribute 'slime-reader-conditional-face nil ; inherits comment face
                      :foreground "burlywood4")

  (set-face-attribute 'slime-inspector-action-face nil
                      :foreground "orange1" :slant 'normal :weight 'bold)
  (set-face-attribute 'slime-inspector-value-face nil
                      :foreground "forest green" :slant 'italic :weight 'normal))

;;; SLIME-only keys
(defun define-my-slime-keys ()
  (slime-define-keys slime-edit-value-mode-map ; bug workaround
    ((kbd "q") 'self-insert-command)
    ((kbd "S-q") 'self-insert-command))
  ;; ****** keys that work in REPL only (can override keys from generic lisp keymap) ******
  (slime-define-keys slime-repl-mode-map
    ((kbd "C-<return>") (lambda ()
                          (interactive)
                          (end-of-buffer)
                          (let ((end (point))
                                (begin (marker-position slime-repl-input-start-mark)))
                            (font-lock-fontify-region begin end)
                            (goto-char end))
                          (slime-repl-closing-return)))
    ((kbd "<return>") 'slime-repl-newline-and-indent)
    ((kbd "<pause> <backspace>") (lambda () (interactive)
                                    (end-of-buffer) (slime-repl-delete-current-input)))
    ((kbd "<M-up>") 'slime-repl-previous-input)
    ((kbd "<M-down>") 'slime-repl-next-input)
    ((kbd "<C-up>") 'backward-up-list)
    ((kbd "<C-down>") 'down-list)
    ((kbd "<M-S-up>") 'slime-repl-previous-prompt)
    ((kbd "<M-S-down>") 'slime-repl-next-prompt)
    ((kbd "<pause> <delete>") (lambda () (interactive) (end-of-buffer) (slime-repl-clear-output)))
    ((kbd "<C-pause>") 'slime-repl-clear-buffer))
  ;; ****** keys that work in all Common Lisp buffers ******
  (slime-define-keys lisp-mode-map
    ((kbd "C-<return>") 'slime-eval-last-expression-in-repl)
    ((kbd "M-<return>") 'slime-eval-print-last-expression))
  ;; ****** keys that work in all Lisp buffers ******
  (slime-define-keys lisp-mode-shared-map
    ;;((kbd "C-\\") 'indent-current-sexp-or-selection)
    ;;((kbd "<M-left>") 'backward-up-list)
    ;;((kbd "<M-right>") 'up-list)
    ;;((kbd "<M-down>") 'down-list)
    ((kbd "<M-up>") 'beginning-of-defun)
    ((kbd "<M-down>") 'end-of-defun)))

(setf *lambda-logo*
   (list
    "                         ...                                        "
    "                      .:kKXXOo.                          ..         "
    "        ;d;          .kWMWWWMMK;                         ,xo.       "
    "      .oOl.         .xXkc;;:xXMK;                         ,kO;      "
    "     'k0:           ;Oc      ;0Wk.                         .kKc     "
    "    'OK;            :l.       ;KNl                          .kXl.   "
    "   'OXc                        lNO.                          ,KXc   "
    "  .xWx.                        .ONl.                          oNK;  "
    "  cNX:                         .dWOc.                         ,0Wx. "
    " .OMO.                         ,0MNKc                         .xMX: "
    " :XMx.                        'OMMMWk.                         oWWd "
    " oWWd                        'OWMMMMX:                         lNMk."
    ".xMWo                       .kWMMWNNWx.                        cNMO."
    ".xMWo                      .xWMMWxlxKK;                        cNMO."
    ".dWWo                     .xWMMWx..,xWd.                       lWMx."
    " cNMx.                   .dNMMWO.   :X0,                       dWNl "
    " '0MO.                  .oNMMM0'    .kNo                      .kM0, "
    "  oNX:                  lNMMMK;      cN0'                     ;KNo  "
    "  .kWx.                lXMMMK:       .OWd.       ..          .dWk.  "
    "   ,0Xc               cXMMMXc         lNXc       cd.         cX0,   "
    "    ;00;             :KMMMNl          .OMXd'   .:0d.        ;00,    "
    "     ,O0:           ;KMMMNd.           ;KMWXOxx0NK;        :0k'     "
    "     .d0l.         ;0MMMWx.             ;ONMMMMWO;       .oOl.      "
    "       ;d:         ':ccc;.               .'cool;.        ,l,        "))

(defun print-hello-message ()
  (let* ((tab-string "         ")
         (lambda-logo-string
          (apply 'concat
                 (append ;; (list "\n")
                  (mapcar (lambda (line)
                            (concat tab-string line "\n"))
                          *lambda-logo*)
                  (list "\n"))))
         (width (+ (length tab-string) (length (first *lambda-logo*))))
         (height (length *lambda-logo*))
         (prompt-string "CL-USER> ")
         (quine-string ; \u03BB is a lambda letter
          "((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))\n")
         (comment-string ";; Elegant weapons, for a more... civilized age.\n"))
    (goto-char 1) ; 19
    ;; (insert "\n\n")
    (put-text-property 0 (length lambda-logo-string)
                       'font-lock-face (list :foreground "white" :background "gray6"
                                             :weight 'bold :slant 'italic)
                       lambda-logo-string)
    (insert lambda-logo-string)
    (let ((pos (point)))
      (dotimes (line height)
        (let ((color (cond ((< line (+ 2 (/ height 3))) "dark red")
                           ((< line (+ 2 (* 2 (/ height 3)))) "gold")
                           (t "forest green"))))
          (put-text-property (+ 25 (* line (1+ width))) (+ 64 (* line (1+ width)))
                             'font-lock-face (list :foreground color :background "gray6"
                                                   :weight 'bold :slant 'italic))))
      (goto-char pos))
    (insert "\n")
    (put-text-property 0 (length comment-string)
                       'font-lock-face 'slime-repl-output-face
                       comment-string)
    (insert comment-string)
    (put-text-property 0 (length prompt-string)
                       'font-lock-face 'slime-repl-prompt-face
                       prompt-string)
    (put-text-property 0 (length prompt-string)
                       'slime-repl-prompt t
                       prompt-string)
    (insert prompt-string)
    (put-text-property 0 (length quine-string)
                       'font-lock-face 'slime-repl-input-face
                       quine-string)
    (insert quine-string)
    (insert "\n")
    ;;(right-char 18)
    ;;(insert "\n")
    (put-text-property 1 (- (point-max) 9) 'read-only t)
    (end-of-buffer)
    ;;(insert "(format t \"Ok, a new REPL is started, let's hack!\")")
    (insert "(lisp-implementation-type)")
    (slime-repl-closing-return)))

(defun animate-lambda ()
  (interactive)
  (let ((animation-buffer-name "Lambda in Parens"))
    (animate-sequence *lambda-logo* 0)))

;; (defun animate-lambda (vpos hpos)
;;   (mapcar (lexical-let ((current-vpos vpos))
;;             (lambda (line)
;;               (animate-string line current-vpos hpos)
;;               (incf current-vpos)))
;;           *lambda-logo*))

(defvar slime-first-startup t)
(add-hook 'slime-connected-hook
          (lambda ()
            (slime-load-file (concat (default-value 'default-directory) "init/ivanp7.lisp"))
            (when slime-first-startup
              (slime-startup-time-init)
              (configure-slime-faces)
              (rainbow-identifiers-load-tune)
              (tabbar-mode 1)
              (define-my-slime-keys)
              ;; (slime-load-file (concat default-directory "init/ivanp7-welcome.lisp"))
              (slime-scratch) ; autocreate *slime-scratch* buffer
              (insert
               (concat
                ";; This is a scratch buffer for Common Lisp evaluation.\n"
                ";; Press <Alt+Enter> to evaluate expression and print result at point.\n"
                ";; Press <F4> to evaluate expression without printing result.\n"
                "\n"))
              (slime-repl)
              (print-hello-message)
              ;;(play-sound-file (concat default-directory "init/ready.wav"))
              (setq slime-first-startup nil)
              ;; Display load time
              (loading-time/stop-timer)
              (run-at-time "1 sec" nil 'anarcat/display-timing))))

;;;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t) ;; automatically update list of functions in the buffer
;;(setq imenu-use-popup-menu nil) ; enable Imenu dialogs in the minibuffer only

(defun try-to-add-imenu ()
  (condition-case nil (imenup-add-defs-to-menubar) (error nil))) ; enable iMenu+
;;(condition-case nil (imenu-add-to-menubar "IMenu") (error nil))) ; not iMenu
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;;;; Pos-Tip for auto-complete
(require 'pos-tip)

;;;; Auto-complete
(require 'auto-complete)

(setq ac-quick-help-prefer-pos-tip nil) ; pos-tip works not so well on Ubuntu

(setq ac-auto-start nil)
(setq ac-quick-help-delay 1.5)
(setq ac-auto-show-menu 0.3)

(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map (kbd "<return>") 'ac-complete)

(global-auto-complete-mode t)
(add-to-list 'ac-modes 'lisp-mode)

(setq-default ac-sources '(ac-source-filename ac-source-files-in-current-dir
                           ac-source-words-in-buffer ac-source-words-in-same-mode-buffers))

(add-hook 'emacs-lisp-mode
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-features)
            (add-to-list 'ac-sources 'ac-source-functions)
            (add-to-list 'ac-sources 'ac-source-symbols)
            (add-to-list 'ac-sources 'ac-source-variables)))

(require 'ac-slime)
(add-hook 'slime-mode-hook (lambda () (set-up-slime-ac t)))
(add-hook 'slime-repl-mode-hook (lambda () (set-up-slime-ac t)))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;; Prettify symbols
(require 'pretty-mode)

(global-pretty-mode -1)
(add-hook 'lisp-mode-hook 'pretty-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-mode)

;; Disabling pretty-mode in *slime-macroexpansion* buffer because of bug
;; (defun slime-create-macroexpansion-buffer-advice (func)
;;   (let ((new-buffer (funcall func)))
;;     (when new-buffer
;;       (pop-to-buffer new-buffer)
;;       (pretty-mode -1)
;;       new-buffer)))

;; (advice-add 'slime-create-macroexpansion-buffer :around 'slime-create-macroexpansion-buffer-advice)

;; Disabling case inconsistency because of bug
(setq-default font-lock-keywords-case-fold-search nil)
(add-hook 'lisp-mode-hook (lambda () (setq font-lock-keywords-case-fold-search nil)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq font-lock-keywords-case-fold-search nil)))

;; redefining pretty-patterns:
;; these patterns would be replaced only if separate symbols (neither in comments nor strings)
(defun pretty-patterns ()
  "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)"
  (let* ((lispy '(scheme emacs-lisp lisp clojure jess clips))
         (mley '(haskell tuareg sml fsharp))
         (c-like '(c c++ perl sh python java ess ruby javascript coffee groovy))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       ;;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       (?\u00AC :neg (:logic) (:not "not" ,@lispy))
       ;; duplucation to get around of font-lock bug
       (?\u00AC :neg-up (:logic) (:not-up "NOT" ,@lispy))
       (?\u00AC :neg-up2 (:logic) (:not-up2 "Not" ,@lispy))

       (?\u2227 :wedge (:logic) (:and "and" ,@lispy))
       (?\u2227 :wedge-up (:logic) (:and-up "AND" ,@lispy))
       (?\u2227 :wedge-up2 (:logic) (:and-up2 "And" ,@lispy))

       (?\u2228 :vee (:logic) (:or "or" ,@lispy))
       (?\u2228 :vee-up (:logic) (:or-up "OR" ,@lispy))
       (?\u2228 :vee-up2 (:logic) (:or-up2 "Or" ,@lispy))

       (?\u221A :sqrt (:arithmetic) (:sqrt "sqrt" ,@lispy))
       (?\u221A :sqrt-up (:arithmetic) (:sqrt-up "SQRT" ,@lispy))
       (?\u221A :sqrt-up2 (:arithmetic) (:sqrt-up2 "Sqrt" ,@lispy))

       (?\u005E :expt (:arithmetic) (:expt "expt" ,@lispy))
       (?\u005E :expt-up (:arithmetic) (:expt-up "EXPT" ,@lispy))
       (?\u005E :expt-up2 (:arithmetic) (:expt-up2 "Expt" ,@lispy))

       ;;(?\u0025 :mod (:arithmetic) (:mod "mod" ,@lispy))
       ;;(?\u0025 :mod-up (:arithmetic) (:mod-up "MOD" ,@lispy))

       (?\u00D7 :mult (:arithmetic) (:mult "*" ,@lispy))

       ;;(?\u00F7 :div (:arithmetic) (:div "/" ,@lispy)) ; conflicts with /=
       ;;(?\u2116 :nth (:sets) (:nth "nth" ,@lispy))
       ;;(?\u2116 :nth-up (:sets) (:nth-up "NTH" ,@lispy))

       (?\u2208 :member (:sets) (:member "member" ,@lispy))
       (?\u2208 :member-up (:sets) (:member-up "MEMBER" ,@lispy))
       (?\u2208 :member-up2 (:sets) (:member-up2 "Member" ,@lispy))

       (?\u2200 :every (:sets) (:every "every" ,@lispy))
       (?\u2200 :every-up (:sets) (:every-up "EVERY" ,@lispy))
       (?\u2200 :every-up2 (:sets) (:every-up2 "Every" ,@lispy))

       (?\u2203 :some (:sets) (:some "some" ,@lispy))
       (?\u2203 :some-up (:sets) (:some-up "SOME" ,@lispy))
       (?\u2203 :some-up2 (:sets) (:some-up2 "Some" ,@lispy))

       (?\u2204 :notany (:sets) (:notany "notany" ,@lispy))
       (?\u2204 :notany-up (:sets) (:notany-up "NOTANY" ,@lispy))
       (?\u2204 :notany-up2 (:sets) (:notany-up2 "Notany" ,@lispy))

       (?\u2190 :setf (:equality) (:setf "setf" ,@lispy))
       (?\u2190 :setf-up (:equality) (:setf-up "SETF" ,@lispy))
       (?\u2190 :setf-up2 (:equality) (:setf-up2 "Setf" ,@lispy))
       ))))

(require 'pretty-symbols)
(setq pretty-symbol-categories '(custom)) ; '(lambda relational logical nil)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
;;(add-hook 'lisp-interaction-mode 'pretty-symbols-mode)

;;; Adding custom symbols to the mode:
;; these patterns would be replaced everywhere
(setq pretty-symbol-patterns
      (let ((lisps '(emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode
                     lisp-mode scheme-mode)))
        (append pretty-symbol-patterns
                `((?\u2205 custom "nil" (,@lisps))
                  (?\u2205 custom "NIL" (,@lisps)) ; uppercase duplication to get around of a bug
                  (?\u2260 custom "/=" (,@lisps))
                  ;;(?\u2248 custom "~=" (,@lisps))
                  (?\u2265 custom ">=" (,@lisps))
                  (?\u2264 custom "<=" (,@lisps))
                  (?\u2192 custom "->" (,@lisps))
                  (?\u2190 custom "<-" (,@lisps))
                  (?\u00B1 custom "\\+-" (,@lisps))
                  (?\u2213 custom "-\\+" (,@lisps))
                  (?\u221E custom "\\<inf\\>" (,@lisps))
                  (?\u221E custom "\\<INF\\>" (,@lisps)) ; uppercase duplication to get around of a bug
                  )
                (loop for pair in
                     (cl-pairlis
                      '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                        "theta" "iota" "kappa" "lambda" "mu" "nu" "xi"
                        "omicron" "pi" "rho" "sigmaf" "sigma" "tau"
                        "upsilon" "phi" "chi" "psi" "omega")
                      (mapcar
                       (lambda (x) (make-char 'greek-iso8859-7 x))
                       (number-sequence 97 121)))
                   collect (list (cdr pair) 'custom
                                 (concat "\\<" (car pair) "\\>") lisps)))))

;; Better fonts for special symbols
(defun custom-fonts-for-pretty-symbols ()
  (let ((symbols-fonts
         '(("PragmataPro" ; monospaced
            (#x2208 #x2200 #x2203 #x2204 #x2227 #x2228 #x2213
             ;; member every some notany and or minus-plus
             #x2205 #x2260
             ;; nil /=
             #x2248 #x2264 #x2265 #x221A
             ;; ~= <= >= sqrt
             ))
           ("Fira Code Medium" ; not monospaced, but good-looking
            (#x2192 #x2190 #x221E)
            ;; -> <- inf
            ))))
    (dolist (font-set symbols-fonts)
      (dolist (sym (cadr font-set))
        (set-fontset-font "fontset-default" sym (car font-set) nil nil) ;; -set) nil 'prepend)
        ))))

;; (defun custom-fonts-for-pretty-symbols ()
;;   (let ((symbols-fonts
;;          '(("Monospace" ; for Ubuntu
;;             (#x2208 #x2200 (#x2203 . #x2204) #x2227 #x2228 #x2213))
;;            ("Hasklig Medium" ;"DejaVu Sans Mono" ; for Windows
;;             (#x2205 #x2260 #x221E ; #x2208 #x2200 (#x2203 . #x2204)
;;              ))
;;            ("Hasklig Medium" ;"Consolas" ; for Windows
;;             (#x2248 (#x2264 . #x2265) #x221A #x2192 #x2190 ; #x2116
;;              )))))
;;     (dolist (font-set symbols-fonts)
;;       (dolist (sym (cadr font-set))
;;         (set-fontset-font "fontset-default" sym (car font-set) nil nil) ;; -set) nil 'prepend)
;;         ))))

(custom-fonts-for-pretty-symbols)

;;;; Pretty suscripts support
(require 'magic-latex-buffer)

(defun suscript-jit-prettifier (beg end) ; redefined ml/jit-prettifier
  (let (magic-latex-ignored-properties)
    (goto-char beg)
    (ml/remove-pretty-overlays beg end)
    ;; prettify suscripts
    (save-excursion
      (while (ignore-errors (ml/search-suscript t end))
        (let* ((body-beg (match-beginning 1))
               (body-end (match-end 1))
               (delim-beg (match-beginning 0))
               (delim-end (match-end 0))
               ;; the point can be already prettified in a recursive
               ;; suscript like "a_{b_c}".
               (oldov (ml/overlay-at body-beg 'category 'ml/ov-pretty))
               (oldprop (and oldov (overlay-get oldov 'display)))
               (priority-base (and oldov (or (overlay-get oldov 'priority) 0)))
               (raise-base (or (cadr (assoc 'raise oldprop)) 0.0))
               (height-base (or (cadr (assoc 'height oldprop)) 1.0))
               (ov1 (ml/make-pretty-overlay delim-beg delim-end 'invisible t))
               ;; new overlay must have higher priority than the old
               ;; one.
               (ov2 (ml/make-pretty-overlay
                     body-beg body-end 'priority (when oldov (1+ priority-base)))))
          (cl-case (string-to-char (match-string 0))
            ((?_) (overlay-put
                   ov2 'display
                   `((raise ,(- raise-base 0.2)) (height ,(* height-base 0.8)))))
            ((?^) (overlay-put
                   ov2 'display
                   `((raise ,(+ raise-base 0.2)) (height ,(* height-base 0.8)))))))))))

(define-minor-mode magic-suscript-buffer
    "Redefinition of the magic-latex-buffer mode, that doesn't conflict with lisp-mode."
  :init-value nil
  :global nil
  :lighter " suscript"
  (if magic-suscript-buffer
      (progn
        (jit-lock-mode 1)
        (setq-local font-lock-multiline t)
        (jit-lock-register 'suscript-jit-prettifier)
        (jit-lock-register 'font-lock-fontify-region))
      (jit-lock-unregister 'suscript-jit-prettifier)
      (jit-lock-unregister 'font-lock-fontify-region)
      (ml/remove-pretty-overlays (point-min) (point-max))
      (font-lock-refresh-defaults)))

(add-hook 'lisp-mode-hook 'magic-suscript-buffer)
(add-hook 'emacs-lisp-mode-hook 'magic-suscript-buffer)

(add-hook 'latex-mode-hook 'magic-latex-buffer)

;;;; Rainbow identifiers
(require 'rainbow-identifiers)

;; (eval-when-compile
;;   (defmacro rainbow-identifiers--define-faces-custom ()
;;     (let* ((faces '())
;;            (colors ["red" "firebrick" "saddle brown" ; red, pink, brown
;;                           "orange red" "yellow" "dark goldenrod" ; orange, yellow
;;                           "olive drab" "forest green" "lime green" "spring green" ; green
;;                           "cyan" "dodger blue" "slate blue" ; blue
;;                           "dark orchid" "violet red" "magenta" ; purple, magenta
;;                           ])
;;            (light-colors colors)
;;            (dark-colors colors))
;;       (dotimes (i (length colors))
;;         (push `(defface ,(intern (format "rainbow-identifiers-custom-%d" (1+ i)))
;;                    '((((class color) (background dark)) :foreground ,(aref dark-colors i))
;;                      (((class color) (background light)) :foreground ,(aref light-colors i)))
;;                  ,(format "Identifier face #%d" (1+ i))
;;                  :group 'rainbow-identifiers-faces)
;;               faces))
;;       `(progn ,@faces))))
;; (rainbow-identifiers--define-faces-custom)

(setq rainbow-identifiers-cie-l*a*b*-lightness 65)
(setq rainbow-identifiers-cie-l*a*b*-saturation 100)
(setq rainbow-identifiers-cie-l*a*b*-color-count 32)

(setq use-colors-count 28)
(setq use-colors-shift -4)

(setq rainbow-identifiers-face-count rainbow-identifiers-cie-l*a*b*-color-count)

(defun rainbow-identifiers-face-chooser (hash)
  (if (numberp hash)
      (rainbow-identifiers-cie-l*a*b*-choose-face
       ;; hack to get rid of the excess of blue colors
       (* (+ use-colors-shift (mod hash rainbow-identifiers-face-count))
          (/ use-colors-count 1.0
             rainbow-identifiers-cie-l*a*b*-color-count)))
      ;; (intern-soft
      ;;  (concat "rainbow-identifiers-custom-"
      ;;          (number-to-string (1+ (mod hash rainbow-identifiers-face-count)))))
      (list (append (list :foreground "white")
                    (if (eq hash :cl-special) (list :slant 'italic))))))

;; Demonstration of the colors:
'(x00 x01 x02 x03 x04 x05 x06 x07 x08 x09 x10 x11 x12 x13 x14 x15
  x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)

(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-face-chooser)

(load "init-cl-symbols-list.el") ; for cl-symbols-list

(defcustom rainbow-identifiers-custom-list nil
  "User-selected custom hash values for specific symbols.")
(defvar rainbow-identifiers-custom-binary-tree nil)

;; --- Binary tree implementation ------------------------------------------------------------------

(defun make-binary-tree (entry left right)
  (list entry left right))

(defun tree-entry (tree)
  (first tree))

(defun tree-left-branch (tree)
  (second tree))

(defun tree-right-branch (tree)
  (third tree))

(defun lookup-binary-tree (x tree)
  (cond ((null tree) nil)
        ((string-equal x (car (tree-entry tree))) (tree-entry tree))
        ((string-lessp x (car (tree-entry tree)))
         (lookup-binary-tree x (tree-left-branch tree)))
        (t (lookup-binary-tree x (tree-right-branch tree)))))

(defun adjoin-binary-tree (x tree)
  (cond ((null tree) (make-binary-tree x nil nil))
        ((string-equal (car x) (car (tree-entry tree))) tree)
        ((string-lessp (car x) (car (tree-entry tree)))
         (make-binary-tree (tree-entry tree)
                           (adjoin-binary-tree x (tree-left-branch tree))
                           (tree-right-branch tree)))
        (t (make-binary-tree (tree-entry tree)
                             (tree-left-branch tree)
                             (adjoin-binary-tree x (tree-right-branch tree))))))

(defun tree->list (tree &optional exclude-p)
  (cl-labels ((copy-to-list (tree result-list)
                            (if (null tree)
                                result-list
                                (copy-to-list (tree-left-branch tree)
                                              (if (or (null exclude-p) (not (funcall
                                                                             exclude-p
                                                                             (tree-entry tree))))
                                                  (cons (tree-entry tree)
                                                        (copy-to-list (tree-right-branch tree)
                                                                      result-list))
                                                  (copy-to-list (tree-right-branch tree)
                                                                result-list))))))
    (copy-to-list tree nil)))

(defun partial-tree (elts n)
  (if (zerop n)
      (cons nil elts)
      (let* ((left-size (truncate (/ (1- n) 2)))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (1+ left-size)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-binary-tree this-entry left-tree right-tree)
              remaining-elts))))

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun delete-from-binary-tree (x tree)
  (list->tree (tree->list tree (lambda (entry) (string-equal x (car entry))))))

;; --- End of binary tree implementation -----------------------------------------------------------

;; (defun rainbow-identifiers--incremental-hash-function (func identifier)
;;   (let ((hash-value (funcall func identifier)))
;;     (if (not (numberp hash-value))
;;         (if (consp hash-value) (cdr hash-value) hash-value)
;;         (let* ((hashes (mapcar (lambda (id) (mod (funcall func id t)
;;                                           (* 10 rainbow-identifiers-face-count)))
;;                                (loop for i from 1 to (length identifier)
;;                                   collect (substring identifier 0 i))))
;;                (new-hash 0) (n (length hashes)) (q 0.9) (r (/ (- 1 q) (- 1 (expt q n)))))
;;           (round (dolist (h hashes new-hash)
;;                    (setf new-hash (+ new-hash (* h r)))
;;                    (setf r (* r q))))))))

(defun rainbow-identifiers--hash-function (identifier &optional cl-also)
  "Redefined version of the standard 'rainbow-identifiers--hash-function'"
  (let* ((identifier (downcase identifier))
         (record (lookup-binary-tree identifier rainbow-identifiers-custom-binary-tree)))
    (cond
      ((and (not cl-also) (member identifier cl-special-operators-symbols-list)) :cl-special)
      ((and (not cl-also) (member identifier cl-symbols-list)) :cl-standard)
      (record (cdr record)) ;; (cons :tuned (cdr record))
      (t (let* ((hash (secure-hash 'sha1 identifier nil nil t))
                (len (length hash))
                (i (- len rainbow-identifiers--hash-bytes-to-use))
                (result 0))
           (while (< i len)
             (setq result (+ (* result 256) (aref hash i)))
             (setq i (1+ i)))
           result)))))

;; (advice-add 'rainbow-identifiers--hash-function :around
;;             'rainbow-identifiers--incremental-hash-function)

(defvar rainbow-identifiers-tune-delta 1)
(defun rainbow-identifiers-tune ()
  (interactive)
  (if (member major-mode '(lisp-mode))
      (progn
        (let ((sym (thing-at-point 'symbol)))
          (if sym
              (let ((sym (downcase (substring-no-properties sym))))
                (if (not (member sym cl-symbols-list))
                    (let ((record (lookup-binary-tree sym rainbow-identifiers-custom-binary-tree)))
                      (if record
                          (setf (cdr record) (mod (+ rainbow-identifiers-tune-delta (cdr record))
                                                  rainbow-identifiers-face-count))
                          (setf rainbow-identifiers-custom-binary-tree
                                (adjoin-binary-tree
                                 (cons sym (mod (+ rainbow-identifiers-tune-delta
                                                   (rainbow-identifiers--hash-function sym))
                                                rainbow-identifiers-face-count))
                                 rainbow-identifiers-custom-binary-tree))))))))
        (font-lock-fontify-buffer))
      (message "Tune is not allowed in this mode.")))

(defun rainbow-identifiers-cancel-tuning ()
  (interactive)
  (if (member major-mode '(lisp-mode))
      (progn
        (let ((sym (thing-at-point 'symbol)))
          (if sym
              (let ((sym (downcase (substring-no-properties sym))))
                (if (not (member sym cl-symbols-list))
                    (let ((record (lookup-binary-tree sym rainbow-identifiers-custom-binary-tree)))
                      (if record
                          (setf rainbow-identifiers-custom-binary-tree
                                (delete-from-binary-tree
                                 (car record) rainbow-identifiers-custom-binary-tree))))))))
        (font-lock-fontify-buffer))
      (message "Tune is not allowed in this mode.")))

;; load 'rainbow-identifiers-custom-list' from file
(defun rainbow-identifiers-load-tune ()
  (interactive)
  (let ((tuning-file (concat (default-value 'default-directory) "init/rainbow-tuning.el")))
    (if (file-readable-p tuning-file)
        (load-file tuning-file)
        (setq rainbow-identifiers-custom-list nil)))
  (sort rainbow-identifiers-custom-list (lambda (rec1 rec2) (string-lessp (car rec1) (car rec2))))
  (setq rainbow-identifiers-custom-binary-tree (list->tree rainbow-identifiers-custom-list))
  (font-lock-fontify-buffer))

;; save 'rainbow-identifiers-custom-list' to file
(defun rainbow-identifiers-save-tune ()
  (interactive)
  (setq rainbow-identifiers-custom-list (tree->list rainbow-identifiers-custom-binary-tree))
  (write-region
   (format "(setq rainbow-identifiers-custom-list \n  (list\n%s    ))\n"
           (apply 'concat (loop for el in rainbow-identifiers-custom-list collect
                               (format "    (cons %S %S)\n" (car el) (cdr el)))))
   nil (concat (default-value 'default-directory) "init/rainbow-tuning.el")))

;; Customized filter: don't mark numbers CL notation '#\name', and '@' in ',@', mark '|name|' symbols
(defun rainbow-identifiers-filter (beg end)
  (let ((str (buffer-substring-no-properties beg end)) (len (- end beg))
        (prefix2 (buffer-substring-no-properties
                  (max (point-min) (- beg 2)) beg))
        (prefix11 (buffer-substring-no-properties
                   (max (point-min) (- beg 11)) beg))
        (prefix17 (buffer-substring-no-properties
                   (max (point-min) (- beg 17)) beg))
        (prev-char (char-before beg)) (first-char (char-after beg)) (second-char (char-after (+ beg 1)))
        (third-char (char-after (+ beg 2))) (prev-last-char (char-before (- end 1)))
        (last-char (char-before end)) (next-char (char-after end)))
    (cond
      ((and (equal prev-char ?\|) (equal next-char ?\|)) t)
      ((or (and (= len 1) (equal first-char ?\.))
           ;; (and (equal first-char ?\@) (equal prev-char ?\,))
           (equal prefix2 "#\\") (equal prev-char ?\#)
           (and (or (equal (upcase prefix11) "#<FUNCTION ")
                    (equal (upcase prefix17) "#<STANDARD-CLASS ")) (equal last-char ?\>))
           (and (equal first-char ?\{) (equal prev-last-char ?\}) (equal last-char ?\>))
           (member first-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
           (and (>= len 2) (member first-char '(?+ ?- ?\.))
                (member second-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
           (and (>= len 3) (member first-char '(?+ ?-)) (equal second-char ?\.)
                (member third-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))) nil)
      (t t))))

(add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)

;; Filter: don't mark identifiers inside comments or strings
(setq rainbow-identifiers-faces-to-override
      '(font-lock-type-face
        font-lock-variable-name-face
        font-lock-constant-face
        font-lock-function-name-face
        font-lock-keyword-face
        font-lock-builtin-face))

;; Installing hooks
(add-hook 'lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)

;;;; Highlight backquoted sexps mode support
(require 'highlight-stages)

;;;; Highlight current sexp
(require 'hl-sexp)

;; Modified version
(defun hl-sexp-highlight ()
  "Active the Hl-Sexp overlay on the current sexp in the current window.
\(Unless it's a minibuffer window.)"
  (when hl-sexp-mode                    ; Could be made buffer-local.
    (unless (or (use-region-p)
                (window-minibuffer-p (selected-window))) ; silly in minibuffer
      (unless hl-sexp-overlay
        (setq hl-sexp-overlay (make-overlay 1 1)) ; to be moved
        (overlay-put hl-sexp-overlay 'face 'hl-sexp-face))
      (overlay-put hl-sexp-overlay 'window (selected-window))
      (save-excursion
        (condition-case nil
            (backward-up-list 1)
          (error nil))
        (let ((bounds (bounds-of-thing-at-point 'sexp)))
          (when bounds
            (move-overlay hl-sexp-overlay
                          (car bounds) (cdr bounds)
                          (current-buffer))))))))

(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'slime-repl-mode-hook 'hl-sexp-mode)

;;;; Highlight matching parens
(set-face-background 'show-paren-match "red") ;; "steelblue4"
(set-face-foreground 'show-paren-match "white")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(set-face-background 'show-paren-mismatch "yellow")
(set-face-foreground 'show-paren-mismatch "white")
(set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

;;;; Paren face
(require 'paren-face)
(global-paren-face-mode)

;;;; Highlight symbol
(require 'highlight-symbol)
(require 'cl-lib)

(setq highlight-symbol-highlight-single-occurrence nil)
(setq highlight-symbol-on-navigation-p t)
(setq highlight-symbol-idle-delay 0.2)
(let ((hook (lambda () (interactive)
                    (cl-pushnew '(highlight-symbol-face :underline t)
                                face-remapping-alist :test 'equal)
                    (highlight-symbol-mode))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook))



(modify-syntax-entry ?@ "'" lisp-mode-syntax-table)

;; (defun thing-at-point-advice (oldfun &rest args)
;;   (let* ((result (apply oldfun args))
;;          (comma+at-sign-detected)
;;          (new-result (if (and (eql (first args) 'sexp)
;;                             (setf at-sign-detected (eql (elt result 0) ?@)))
;;                          (substring result 1)
;;                          result)))
;;     (if (and at-sign-detected (eql (char-after (point)) ?@))
;;         nil
;;         new-result)))

;; (advice-add 'thing-at-point :around 'thing-at-point-advice)

;; (defun bounds-of-thing-at-point-advice (oldfun &rest args)
;;   (let ((result (apply oldfun args)))
;;     (if result
;;         (let ((prefix-shift (if (eql (first args) 'sexp)
;;                                 (if (eql (char-before (car result)) ?@)
;;                                     (if (eql (char-before (1- (car result))) ?,)
;;                                         -2 -1)
;;                                     0)
;;                                 0)))
;;           (cons (+ prefix-shift (car result)) (cdr result))))))

;; (advice-add 'bounds-of-thing-at-point :around 'bounds-of-thing-at-point-advice)

;;;; Hide/Show feature
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(setq hs-minor-mode-menu t)

;;;; Expand region
(require 'expand-region)

;;;; Auto indentation
;;(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode)
(auto-indent-global-mode)

(setq auto-indent-backward-delete-char-behavior 'untabify)

;;;; Buffer move
(require 'buffer-move)
(setq buffer-move-behavior 'move)

;;;; Undo tree
(require 'undo-tree)

(define-global-minor-mode global-undo-tree-mode
    undo-tree-mode
  (lambda () (undo-tree-mode t)))

(global-undo-tree-mode)

;;;; Magit
(require 'magit)
(global-magit-file-mode)

;;;; Change cursor
(require 'cursor-chg)  ; Load the library
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode

(setq curchg-default-cursor-color "white")
(setq curchg-input-method-cursor-color "orange")
(setq curchg-default-cursor-type 'bar)
(setq curchg-idle-cursor-type 'box)
(setq curchg-overwrite/read-only-cursor-type 'hbar)

(setq cursor-in-non-selected-windows 'hollow)
(setq-default cursor-in-non-selected-windows 'hollow)

(curchg-change-cursor-when-idle-interval 10)

;;;; Eldoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; Random idle quote
(require 'random-idle-quote)

(setq random-idle-quote-delay 30)

(random-idle-quote)
