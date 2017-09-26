;;;; Acquiring list of the used packages
(defvar required-packages
  (set-difference
   (mapcar (lambda (filename) (intern (substring filename 0 -3)))
           (directory-files cl-ide-init-ext-path nil
                            "^\\([^_[:space:]]+[[:graph:]]*[.]el\\)$" nil))
   (with-temp-buffer
     (insert-file-contents (concat cl-ide-init-ext-path "_ignored.sexp"))
     (goto-char (point-min))
     (sexp-at-point)))
  "a list of packages to ensure are installed at launch.")

;;;; Initializing package manager and loading used packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; if not all packages are installed, check and install the missing ones.
(let ((packages-to-install (remove-if 'package-installed-p required-packages)))
  (when packages-to-install
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p packages-to-install)
      (package-install p))))

;;;; Loading configuration scripts
(load (concat cl-ide-init-ext-path "_generic.el"))
(load (concat cl-ide-init-ext-path "_built-in.el"))
(load (concat cl-ide-init-ext-path "_3rdparty.el"))

;; Load configuration scripts for required packages
(dolist (pkg required-packages)
  (load (concat cl-ide-init-ext-path (symbol-name pkg) ".el")))
