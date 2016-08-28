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
(defvar cl-ide-path)
(defvar cl-ide-code-path)
(defvar cl-ide-init-path)

(cond
  ((system-is-windows)
   (setq cl-ide-path "d:/common-lisp/"))
  ((system-is-linux)
   (setq cl-ide-path "~/common-lisp/")))

(setq cl-ide-code-path (concat cl-ide-path "code/"))
(setq cl-ide-init-path (concat cl-ide-path "init/"))

(setq default-directory (setq-default default-directory cl-ide-code-path))
(add-to-list 'load-path cl-ide-init-path)
(add-to-list 'load-path (concat cl-ide-init-path "elisp/"))
(add-to-list 'custom-theme-load-path (concat cl-ide-init-path "color-themes/"))

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

;;;; Starting IDE, setting up windows configuration
(setq ide-started nil)

(defun start-cl-ide (&optional cl-implementation prompt-prefix-text)
  (interactive)

  (when ide-started
    (return-from start-cl-ide))

  ;;; Maximize frame
  (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
      (modify-frame-parameters nil `((maximized . maximized)))
      (modify-frame-parameters nil `((fullscreen . maximized))))

  (if cl-implementation
      (setq slime-default-lisp cl-implementation)
      (flet ((make-enumeration-string (lst)
               (apply 'concat (cons (first lst)
                                    (mapcar (lambda (str) (concat ", " str))
                                            (cdr lst))))))
        (let* ((implementations (mapcar (lambda (impl) (prin1-to-string (car impl)))
                                        slime-lisp-implementations))
               (prompt (concat (if (plusp (length prompt-prefix-text))
                                   (concat "[" prompt-prefix-text "] ")
                                   "")
                               "Common Lisp implementation to start (default: "
                               (car implementations) "; alternatives: "
                               (make-enumeration-string (cdr implementations)) "): "))
               (result (read-from-minibuffer prompt)))
          (setq slime-default-lisp
                (if (member result implementations)
                    (intern result)
                    (intern (first implementations)))))))
  (timer/start)

  (delete-other-windows)
  (split-window-horizontally)
  (slime) ;;(let ((current-prefix-arg -1)) (slime))
  (split-window-vertically (truncate (* 0.75 (window-body-height))))
  (switch-window--jump-to-window 3) ;;(other-window 1)
  ;; (eshell)
  (ielm)
  (ansi-term "/bin/bash")
  (switch-window--jump-to-window 1) ;;(other-window 1)

  ;;;; Open org-mode files
  ;;(find-file (concat (default-value 'default-directory) "../info.org"))
  (find-file (concat org-directory "ivanp7.org"))

  ;; (desktop-read) ;; Load default desktop from file : "~/emacs.d/.emacs.desktop"
  (setq ide-started t))

(add-hook 'window-setup-hook 'timer/stop t)
(add-hook 'window-setup-hook (lambda () (start-cl-ide nil (concat (format "%.2f" *timer*) " sec"))) t)
