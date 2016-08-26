;;;; Starting countdown of "init.el" loading time
(defvar *timer* 0)
(defvar *timer-start-time* nil)

(defun timer/time-to-s (time)
  (/ (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time))))
     1000000.0))

(defun timer/start ()
  (setq *timer-start-time* (current-time)))

(defun timer/stop ()
  (if *timer-start-time*
      (incf *timer* (- (timer/time-to-s (current-time))
                       (timer/time-to-s *timer-start-time*)))))

(defun timer/reset ()
  (setq *timer-start-time* nil)
  (setq *timer* 0))

(defun anarcat/display-timing ()
  (message "------------------------------------------")
  (let ((result *timer*))
    (if *timer-start-time*
        (message "Emacs IDE loaded in %.2f seconds" result)
        (message "Emacs IDE loading time is unknown"))))

(timer/start)

;;;; Setting personal data
(setq user-full-name   "ivanp7")
(setq user-mail-adress "podmazov@gmail.com")

;;;; System-type definition functions
(defun system-is-linux ()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
  (string-equal system-type "windows-nt"))

;;;; Setting up load-path and default-directory
(cond
  ((system-is-windows)
   (setq-default default-directory "d:/common-lisp/ivanp7/")
   (setq default-directory "d:/common-lisp/ivanp7/"))
  ((system-is-linux)
   (setq-default default-directory "~/common-lisp/ivanp7/")
   (setq default-directory "~/common-lisp/ivanp7/")))

(add-to-list 'load-path (concat default-directory "init/"))
(add-to-list 'load-path (concat default-directory "init/elisp/"))

;;;; Configuring Emacs
(load "init-emacs.el")

;;;; Initializing extensions
(load "init-extensions.el")

;;;; Setting up a minor mode for useful key shortcuts
(defconst menu-key-name
  (cond
    ((system-is-windows) "<apps>")
    ((system-is-linux) "<menu>")))

(load "init-keys.el")
(load "init-buffer-keys.el")
(load "init-lisp-mode-keys.el")

;; Starting server
(or (server-running-p)
   (server-start))

(add-hook 'window-setup-hook
          (lambda ()
            (interactive)

            ;;; Maximize frame
            (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
                (modify-frame-parameters nil `((maximized . maximized)))
                (modify-frame-parameters nil `((fullscreen . maximized))))

            ;;;; Starting IDE, setting up windows configuration
            (progn
              (timer/stop)

              (let* ((implementations (mapcar (lambda (impl) (prin1-to-string (car impl)))
                                              slime-lisp-implementations))
                     (impl-enum-string
                      (apply 'concat (cons (first implementations)
                                           (mapcar (lambda (impl) (concat ", " impl))
                                                   (cdr implementations)))))
                     (prompt (concat "Common Lisp implementation to start (" impl-enum-string "): "))
                     (result (read-from-minibuffer prompt)))
                (setq slime-default-lisp
                      (if (member result implementations)
                          (intern result)
                          (intern (first implementations)))))

              (timer/start))

            (split-window-horizontally)
            (slime) ;;(let ((current-prefix-arg -1)) (slime))
            (split-window-vertically (truncate (* 0.75 (window-body-height))))
            (switch-window--jump-to-window 3) ;;(other-window 1)
            ;; (eshell)
            (ielm)
            (ansi-term "/bin/bash")
            (switch-window--jump-to-window 1) ;;(other-window 1)

            ;;;; Open org-mode files
            ;;(find-file (concat (default-value 'default-directory) "info.org"))
            (find-file (concat (default-value 'default-directory) "ivanp7.org"))

            ;; (desktop-read) ;; Load default desktop from file : "~/emacs.d/.emacs.desktop"
            )
          t) ; always maximize window on startup
