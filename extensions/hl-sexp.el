;;;; Highlight current sexp
(require 'hl-sexp)

;; Modified version
(defun hl-sexp-highlight ()
  "Active the Hl-Sexp overlay on the current sexp in the current window.
\(Unless it's a minibuffer window.)"
  (when hl-sexp-mode                    ; Could be made buffer-local.
    (unless (or (use-region-p)
               (window-minibuffer-p (selected-window))) ; silly in minibuffer
      (unless hl-sexp-overlay
        (setq hl-sexp-overlay (make-overlay 1 1)) ; to be moved
        (overlay-put hl-sexp-overlay 'face 'hl-sexp-face))
      (overlay-put hl-sexp-overlay 'window (selected-window))
      (save-excursion
        (condition-case nil
            (backward-up-list 1)
          (error nil))
        (let ((bounds (bounds-of-thing-at-point 'sexp)))
          (when bounds
            (move-overlay hl-sexp-overlay
                          (car bounds) (cdr bounds)
                          (current-buffer))))))))

(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'slime-repl-mode-hook 'hl-sexp-mode)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                           (char-equal (char-syntax cb) ?\) )
                           (blink-matching-open))))
    (when matching-text (message matching-text))))

(set-face-background 'hl-sexp-face "gray20" nil)
