;;;; Starting countdown of "init.el" loading time
(defvar *emacs-started-time* nil)
(defvar *emacs-loaded-time* nil)

(defun loading-time/start-timer ()
  (setq *emacs-started-time* (current-time)))

(defun loading-time/stop-timer ()
  (setq *emacs-loaded-time* (current-time)))

(defun anarcat/time-to-s (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))

(defun loading-time/get-seconds ()
  (if (and *emacs-started-time* *emacs-loaded-time*)
      (/ (- (anarcat/time-to-s *emacs-loaded-time*)
            (anarcat/time-to-s *emacs-started-time*)) 1000000.0)))

(defun anarcat/display-timing ()
  (message "------------------------------------------")
  (let ((result (loading-time/get-seconds)))
    (if result
        (message "Emacs loaded in %.2f seconds" result)
        (message "Emacs loading time is unknown"))))

(loading-time/start-timer)

;;;; Setting personal data
(setq user-full-name   "ivanp7")
(setq user-mail-adress "ivanp7@mail.ru")

;;;; System-type definition functions
(defun system-is-linux ()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
  (string-equal system-type "windows-nt"))

;;;; Setting up load-path and default-directory
(cond
  ((system-is-windows)
   (setq-default default-directory "d:/lisp/ivanp7/")
   (setq default-directory "d:/lisp/ivanp7/"))
  ((system-is-linux)
   (setq-default default-directory "~/lisp/ivanp7/")
   (setq default-directory "~/lisp/ivanp7/")))

(add-to-list 'load-path (concat default-directory "init/"))
(add-to-list 'load-path (concat default-directory "init/elisp/"))

;;;; Configuring Emacs
(load "init-emacs.el")

;;;; Initializing extensions
(load "init-extensions.el")

;;;; Setting up a minor mode for useful key shortcuts
(load "init-keys.el")

;;;; Starting server
(or (server-running-p)
   (server-start))

;;;; Setting up windows configuration
(split-window-horizontally)
(slime)
(split-window-vertically 23)
(other-window 1)
;; (eshell)
(ielm)
(ansi-term "/bin/bash")
(other-window 1)

;; open org-mode files
(find-file (concat (default-value 'default-directory) "info.org"))
(find-file (concat (default-value 'default-directory) "ivanp7.org"))

;; (desktop-read) ;; Load default desktop from file : "~/emacs.d/.emacs.desktop"
