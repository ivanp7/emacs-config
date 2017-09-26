;;;; Starting up IDE layout

;; IDE setup function
(defvar ide-layout-setup-fn-version 3)
(defvar ide-layout-setup-fn
  (nth
   (1- ide-layout-setup-fn-version)
   (list
    (lambda ()
      (interactive)
      (find-file (concat org-directory "ivanp7.org"))
      (split-window-horizontally)
      (switch-window--jump-to-window 2)
      (setq slime-repl-print-logo t)
      (slime) ;;(let ((current-prefix-arg -1)) (slime))
      (split-window-vertically (truncate (* 0.75 (window-body-height))))
      (switch-window--jump-to-window 3) ;;(other-window 1)
      ;; (eshell)
      (ielm)
      (let ((default-directory cl-ide-code-path))
        (ansi-term "/bin/bash")))
    (lambda ()
      (interactive)
      (find-file (concat org-directory "ivanp7.org"))
      (split-window-vertically (truncate (* 0.75 (window-body-height))))
      (split-window-horizontally)
      (switch-window--jump-to-window 3)
      (split-window-horizontally)
      (slime)
      (switch-window--jump-to-window 4)
      (ielm)
      (let ((default-directory cl-ide-code-path))
        (ansi-term "/bin/bash"))
      (switch-window--jump-to-window 1)
      (switch-to-slime-scratch)
      (setq slime-scratch-text (concat slime-scratch-text
                                       (make-lambda-logo-string ";;    "))))
    (lambda ()
      (interactive)
      (let ((N 3))
        (dotimes (i (1- N))
          (split-window-horizontally))
        (dotimes (i N)
          (split-window-vertically (truncate (* 0.5 (window-body-height))))
          (other-window 2))
        (balance-windows)
        
        (switch-window--jump-to-window (- (* 2 N) 3))
        (find-file "notes.org")
        
        (other-window 1)
        (find-file (concat org-directory "ivanp7.org"))
        
        (other-window 1)
        (slime)
        
        (switch-window--jump-to-window (* 2 N))
        (ielm)
        (let ((default-directory cl-ide-code-path))
          (ansi-term "/bin/bash"))
        
        (setq slime-scratch-text (concat slime-scratch-text
                                         (make-lambda-logo-string ";;    ")))
        
        (dotimes (i (- (* 2 N) 4))
          (switch-window--jump-to-window (+ i 1))
          (switch-to-slime-scratch)))))))

;;; IDE initialization
(with-temp-buffer
  (insert-file-contents (concat cl-ide-init-aux-path
                                "cl-implementations.sexp"))
  (goto-char (point-min))
  (let ((impl-list (sexp-at-point)))
    (setq slime-lisp-implementations
          (getf impl-list :implementations))
    (setq slime-default-lisp
          (getf impl-list :default))))


(setq ide-started nil)

(defun start-cl-ide (&optional cl-implementation prompt-prefix-text)
  (interactive)
  (unless ide-started
    ;;; Maximize frame
    (maximize-frame)
    
    (setq slime-default-lisp
          (if cl-implementation
              cl-implementation
              (cl-flet ((make-enumeration-string
                         (lst)
                         (apply 'concat (cons (first lst)
                                              (mapcar (lambda (str) (concat ", " str))
                                                      (cdr lst))))))
                (let* ((implementations
                        (mapcar (lambda (impl) (prin1-to-string (car impl)))
                                slime-lisp-implementations))
                       (prompt
                        (concat (if (plusp (length prompt-prefix-text))
                                    (concat "[" prompt-prefix-text "] ")
                                    "")
                                "Common Lisp implementation to start (default: "
                                (car implementations) "; alternatives: "
                                (make-enumeration-string (cdr implementations))
                                "): "))
                       (result (completing-read prompt implementations)))
                  (if (member result implementations)
                      (intern result)
                      (intern (first implementations)))))))
    
    (timer/start)
    
    (delete-other-windows)
    (funcall ide-layout-setup-fn)
    
    ;; (desktop-read) ; Load default desktop from file : "~/emacs.d/.emacs.desktop"
    (setq ide-started t)))
