;;;; SLIME
(require 'slime-autoloads)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq slime-contribs '(slime-fancy slime-repl-ansi-color))
(setq slime-repl-ansi-color t)

(setq slime-net-coding-system 'utf-8-unix)

;; Optionally, specify the lisp program you are using. Default is "lisp"
;; (setq inferior-lisp-program "sbcl")

;; Common Lisp HyperSpec
(setq common-lisp-hyperspec-root (expand-file-name "../books/HyperSpec/"))

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

;;; SLIME faces
(defun switch-to-and-fontify-repl-input ()
  (interactive)
  (pop-to-buffer (slime-repl-buffer))
  (end-of-buffer)
  (let ((end (point))
        (begin (marker-position slime-repl-input-start-mark)))
    (font-lock-fontify-region begin end)
    (goto-char end)))

(defun copy-expression-to-repl ()
  "Copy-Expression-To-Repl-And-Eval"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp))
        (edit-buffer (current-buffer))
        (pos (point))
        (lisp-buffer (slime-repl-buffer)))
    (if (null bounds)
        (message "There is no expression at point.")
        (progn
          (pop-to-buffer lisp-buffer)
          (end-of-buffer)
          (slime-repl-delete-current-input)
          (pop-to-buffer edit-buffer)
          (append-to-buffer lisp-buffer (car bounds) (cdr bounds))
          (switch-to-and-fontify-repl-input)))))

(defun configure-slime-faces ()
  (set-face-attribute 'slime-repl-input-face nil
                      :foreground "light goldenrod"
                      :slant 'normal :weight 'normal)
  (set-face-attribute 'slime-repl-output-face nil ; inherits string face
                      :foreground "steel blue"
                      :slant 'italic :weight 'normal)
  (set-face-attribute 'slime-repl-inputed-output-face nil
                      :foreground "firebrick"
                      :slant 'italic :weight 'normal)
  (set-face-attribute 'slime-repl-output-mouseover-face nil
                      :box "chocolate")
  (set-face-attribute 'slime-repl-result-face nil
                      :foreground "chocolate"
                      :slant 'italic)
  (set-face-attribute 'slime-repl-prompt-face nil ; inherits keyword face
                      :foreground "sea green"
                      :slant 'normal :weight 'bold)
  
  (set-face-attribute 'slime-reader-conditional-face nil ; inherits comment face
                      :foreground "burlywood4")
  
  (set-face-attribute 'slime-inspector-action-face nil
                      :foreground "orange1"
                      :slant 'normal :weight 'bold)
  (set-face-attribute 'slime-inspector-value-face nil
                      :foreground "forest green"
                      :slant 'italic :weight 'normal))

;;; SLIME-only keys
(defun define-my-slime-keys ()
  (slime-define-keys slime-edit-value-mode-map ; bug workaround
    ((kbd "q") 'self-insert-command)
    ((kbd "S-q") 'self-insert-command))
  ;; * keys that work in REPL only (can override keys from generic lisp keymap) *
  (slime-define-keys slime-repl-mode-map
    ((kbd "C-<return>") (lambda ()
                          (interactive)
                          (switch-to-and-fontify-repl-input)
                          (slime-repl-closing-return)))
    ((kbd "<return>") 'slime-repl-newline-and-indent)
    ((kbd "<M-up>") 'slime-repl-previous-input)
    ((kbd "<M-down>") 'slime-repl-next-input)
    ((kbd "<C-up>") 'backward-up-list)
    ((kbd "<C-down>") 'down-list)
    ((kbd "<M-S-up>") 'slime-repl-previous-prompt)
    ((kbd "<M-S-down>") 'slime-repl-next-prompt)
    ((kbd "C-] <backspace>")
     (lambda () (interactive)
        (end-of-buffer)
        (slime-repl-delete-current-input)))
    ((kbd "C-] <delete>")
     (lambda () (interactive)
        (end-of-buffer)
        (slime-repl-clear-output)))
    ((kbd "C-] <return>") 'slime-repl-clear-buffer))
  ;; ****** keys that work in all Common Lisp buffers ******
  (slime-define-keys lisp-mode-map
    ((kbd "C-<return>")
     (lambda () (interactive)
        (copy-expression-to-repl)
        (slime-repl-closing-return)))
    ((kbd "M-<return>") 'slime-eval-print-last-expression))
  ;; ****** keys that work in all Lisp buffers ******
  (slime-define-keys lisp-mode-shared-map
    ;;((kbd "C-\\") 'indent-current-sexp-or-selection)
    ;;((kbd "<M-left>") 'backward-up-list)
    ;;((kbd "<M-right>") 'up-list)
    ;;((kbd "<M-down>") 'down-list)
    ((kbd "<M-up>") 'beginning-of-defun)
    ((kbd "<M-down>") 'end-of-defun)))

(defvar *lambda-logo*
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

(defun make-lambda-logo-string (tab-string)
  (apply 'concat
         (append ;; (list "\n")
          (mapcar (lambda (line)
                    (concat tab-string line "\n"))
                  *lambda-logo*)
          (list "\n"))))

(defun print-hello-message ()
  (let*
      ((tab-string "         ")
       (lambda-logo-string (make-lambda-logo-string tab-string))
       (width (+ (length tab-string) (length (first *lambda-logo*))))
       (height (length *lambda-logo*))
       (prompt-string "CL-USER> ")
       (quine-string ; \u03BB is a lambda letter
        "((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))\n")
       (comment-string ";; Elegant weapons, for a more... civilized age.\n"))
    (goto-char 1) ; 19
    ;; (insert "\n\n")
    (put-text-property 0 (length lambda-logo-string)
                       'font-lock-face
                       (list :foreground "white" :background "gray6"
                             :weight 'bold :slant 'italic)
                       lambda-logo-string)
    (insert lambda-logo-string)
    (let ((pos (point)))
      (dotimes (line height)
        (let ((color (cond ((< line (+ 2 (/ height 3))) "dark red")
                           ((< line (+ 2 (* 2 (/ height 3)))) "gold")
                           (t "forest green"))))
          (put-text-property
           (+ 25 (* line (1+ width))) (+ 64 (* line (1+ width)))
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
    (put-text-property 1 (- (point-max) 9) 'read-only t)))

(defun slime-repl-send-initial-command ()
  (end-of-buffer)
  ;;(insert "(format t \"Ok, a new REPL is started, let's hack!\")")
  (insert "(list (lisp-implementation-type) (lisp-implementation-version))")
  (slime-repl-closing-return))

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

(defvar slime-repl-print-logo nil)

(defvar slime-scratch-text
  (concat
   ";; This is a scratch buffer for Common Lisp evaluation.\n"
   ";; Press <Alt+Enter> to evaluate expression and print result at point.\n"
   ";; Press <F4> to evaluate expression without printing result.\n"
   "\n"))

(defun switch-to-slime-scratch ()
  (set-buffer (slime-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (switch-to-buffer (current-buffer))))

(defvar slime-first-startup t)
(add-hook 'slime-connected-hook
          (lambda ()
            (slime-load-file (concat cl-ide-init-path "cl-init.lisp"))
            (when slime-first-startup
              (slime-set-default-directory (default-value 'default-directory))
              (configure-slime-faces)
              (define-my-slime-keys)
              ;; (slime-load-file (concat default-directory
              ;;                          "init/ivanp7-welcome.lisp"))
              
              ;; Silently autocreate *slime-scratch* buffer and fill it
              (with-current-buffer (slime-scratch-buffer)
                (setq default-directory (default-value 'default-directory))
                (insert slime-scratch-text))
              (slime-repl) ; switch to REPL
              (when slime-repl-print-logo
                (print-hello-message))
              (slime-repl-send-initial-command)
              ;;(play-sound-file (concat default-directory "init/ready.wav"))
              (setq slime-first-startup nil)
              ;; Display load time
              (timer/stop)
              (run-at-time "1 sec" nil
                           (lambda ()
                             (timer/display-timing)
                             (timer/reset))))))
