(require 'cl)

;;;; System-type detection functions
(defun system-is-linux ()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
  (string-equal system-type "windows-nt"))

;;;; Specifying paths
(defvar cl-ide-path
  (cond
    ((system-is-windows) "d:/common-lisp/")
    ((system-is-linux) "~/common-lisp/")))
(defvar cl-ide-code-path (concat cl-ide-path "code/"))
(defvar cl-ide-init-path (concat cl-ide-path "init/"))

(defvar cl-ide-init-aux-path (concat cl-ide-init-path "aux/"))
(defvar cl-ide-init-ext-path (concat cl-ide-init-path "extensions/"))

(defvar cl-ide-init-keymaps-path (concat cl-ide-init-path "keymaps/"))

(setq default-directory
      (setq-default default-directory cl-ide-code-path))

(add-to-list 'load-path cl-ide-init-path)
(add-to-list 'load-path (concat cl-ide-init-path "3rdparty/"))
(add-to-list 'custom-theme-load-path
             (concat cl-ide-init-path "3rdparty/color-themes/"))

;;;; Setting personal data
(ignore-errors
  (with-temp-buffer
    (insert-file-contents (concat cl-ide-init-aux-path
                                  "credentials.sexp"))
    (goto-char (point-min))
    (let ((credentials (sexp-at-point)))
      (when (getf credentials :login-name)
        (setq user-login-name (getf credentials :login-name)))
      (when (getf credentials :full-name)
        (setq user-full-name (getf credentials :full-name)))
      (when (getf credentials :e-mail)
        (setq user-mail-adress (getf credentials :e-mail))))))

;;;; Starting countdown of Emacs loading time
(load "init-timer.el")
(timer/start)

;;;; Generic Emacs settings
(load "init-emacs.el")

;;;; Initializing extensions
(load "init-extensions.el")

;;;; Initializing keymaps and input
(load "init-input.el")

;; Starting server
(unless (server-running-p)
  (server-start))

;;;; Starting IDE, setting up windows configuration
(load "init-layout.el")
(add-hook 'window-setup-hook 'timer/stop t)
(add-hook 'window-setup-hook
          (lambda ()
            (start-cl-ide nil (concat (format "%.2f" *timer*) " sec")))
          t)
