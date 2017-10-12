;;;; Starting up IDE layout

;; IDE setup function
(defun ide-setup ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer (sly-scratch-buffer))
  
  (let ((N 3))
    ;; split screen to 2N windows
    (dotimes (i (1- N))
      (split-window-horizontally))
    (dotimes (i N)
      (split-window-vertically (truncate (* 0.5 (window-body-height))))
      (other-window 2))
    (balance-windows)
    
    (switch-window--jump-to-window 1)
    
    (dotimes (i (* 2 N))
      (switch-window--jump-to-window (+ i 1))
      (cond
        ((= i (- (* 2 N) 4)) (find-file "notes.org"))
        ((= i (- (* 2 N) 3)) (find-file (concat org-directory "ivanp7.org")))
        ((= i (- (* 2 N) 2)) (switch-to-buffer (sly-mrepl)))
        ((= i (- (* 2 N) 1))
         (ielm)
         (let ((default-directory cl-ide-code-path))
           (ansi-term "/bin/bash")))))
    
    (switch-window--jump-to-window (- (* 2 N) 1))))

;;; IDE initialization
(with-temp-buffer
    (insert-file-contents (concat cl-ide-init-aux-path
                                  "cl-implementations.sexp"))
  (goto-char (point-min))
  (let ((impl-list (sexp-at-point)))
    (setq sly-lisp-implementations
          (getf impl-list :implementations))
    (setq sly-default-lisp
          (getf impl-list :default))))

(setq ide-started nil)

(defun start-cl-ide (&optional cl-implementation prompt-prefix-text)
  (interactive)
  (unless ide-started
    ;;; Maximize frame
    (maximize-frame)
    
    (setq sly-default-lisp
          (if cl-implementation
              cl-implementation
              (cl-flet ((make-enumeration-string
                         (lst)
                         (apply 'concat (cons (first lst)
                                              (mapcar (lambda (str) (concat ", " str))
                                                      (cdr lst))))))
                       (let* ((implementations
                                (mapcar (lambda (impl) (prin1-to-string (car impl)))
                                        sly-lisp-implementations))
                              (prompt
                                (concat
                                 (if (plusp (length prompt-prefix-text))
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
    
    (sly)
    
    ;; (desktop-read) ; Load default desktop from file : "~/emacs.d/.emacs.desktop"
    (setq ide-started t)))
