;;;; SLY

(require 'sly-autoloads)
(add-hook 'lisp-mode-hook (lambda () (sly-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-sly-mode t)))
(add-to-list 'sly-contribs 'sly-repl-ansi-color)
(setq sly-repl-ansi-color t)

(setq sly-net-coding-system 'utf-8-unix)

;;; sly-scatch tweaks
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

(defvar sly-scratch-text
  (concat
   ";; This is a scratch buffer for Common Lisp evaluation.\n"
   ";; Press <Alt+Enter> to evaluate expression and print result at point.\n"
   ";; Press <F4> to evaluate expression without printing result.\n"
   "\n"
   (make-lambda-logo-string ";; ")))

(defun configure-sly-keys ()
  (let ((map sly-mrepl-mode-map))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map [return] 'newline-and-indent)
    (define-key map (kbd "<C-return>") 'sly-mrepl-return)
    (define-key map (kbd "<C-f2>") 'sly-mrepl-indent-and-complete-symbol)
    (define-key map (kbd "TAB") nil)
    (define-key map (kbd "<S-escape>") 'sly-interrupt)
    (define-key map (kbd "C-c C-b") nil)
    (define-key map (kbd "C-c C-c") nil)
    (define-key map (kbd "<M-up>") 'sly-mrepl-previous-input-or-button)
    (define-key map (kbd "<M-down>") 'sly-mrepl-next-input-or-button)
    (define-key map (kbd "M-p") nil)
    (define-key map (kbd "M-n") nil)
    (define-key map (kbd "<M-S-up>") 'sly-button-backward)
    (define-key map (kbd "<M-S-down>") 'sly-button-forward)
    (define-key map (kbd "C-M-p") nil)
    (define-key map (kbd "C-M-n") nil)))

(defun sly-repl-send-initial-command ()
  (end-of-buffer)
  (let ((inhibit-read-only t))
    (insert "(list (lisp-implementation-type) (lisp-implementation-version))"))
  (run-at-time "0.5 sec" nil
               (lambda ()
                 (sly-mrepl--send-input-sexp))))

(defvar sly-initialized nil)
(add-hook 'sly-connected-hook
          (lambda ()
            (unless sly-initialized
              (configure-sly-keys)
              (sly-load-file (concat cl-ide-init-path "cl-init.lisp"))
              (with-current-buffer (sly-scratch-buffer)
                (insert sly-scratch-text))
              (ide-setup)
              (with-current-buffer (sly-mrepl)
                (sly-repl-send-initial-command))
              ;; Stop timer and display load time
              (timer/stop)
              (run-at-time "1 sec" nil
                           (lambda ()
                             (timer/display-timing)
                             (timer/reset)))
              (setq sly-initialized t))))

;;; Redefining functions with bugs
(defadvice sly-mrepl-clear-recent-output
    (around my-sly-mrepl-clear-recent-output ())
  "Clear this REPL's output between current and last prompt."
  (interactive)
  (sly-mrepl--assert-mrepl)
  (cl-loop for search-start =
           (set-marker (make-marker)
                       (1+ (overlay-start sly-mrepl--last-prompt-overlay)))
           then pos
           for pos = (set-marker
                      search-start
                      (previous-single-property-change search-start 'field))
           while (and (marker-position pos)
                    ;; FIXME: fragile (1- pos), use narrowing
                    (not (get-text-property (1- pos) 'sly-mrepl--prompt))
                    (> pos (point-min)))
           when (eq (field-at-pos pos) 'sly-mrepl--output)
           do (let ((inhibit-read-only t))
                (delete-region
                 (field-beginning pos)
                 (field-end pos))
                (sly-mrepl--insert-output "; Cleared last output"
                                          'sly-mrepl-note-face))
           and return nil)
  (sly-message "Cleared last output"))
