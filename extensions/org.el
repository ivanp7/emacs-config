;;;; Org-mode
(require 'org)

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "ivanp7.org"))
(setq org-agenda-files (list (concat org-directory "ivanp7.org")))
(setq org-archive-location (concat org-directory "ivanp7-archive.org" "::"))

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-startup-indented t)
(setq org-M-RET-may-split-line nil)
(setq org-startup-truncated nil)
(setq org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
;;(setq org-startup-folded nil)
(setq org-footnote-auto-adjust t)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<mouse-3>")
              (lambda (event) (interactive "e")
                 (mouse-set-point event) (org-todo)))
            (define-key org-mode-map (kbd "<mouse-2>")
              (lambda (event) (interactive "e")
                 (mouse-set-point event) (org-cycle)))
            (define-key org-mode-map (kbd "<S-mouse-2>")
              (lambda (event) (interactive "e")
                 (mouse-set-point event) (org-shifttab)))))

(setq org-todo-keywords
      '((sequence "TODO" "DELAYED" "INPROGRESS" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DELAYED" . (:foreground "peru" :weight bold))
        ("INPROGRESS" . (:foreground "OrangeRed1" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold))))

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
