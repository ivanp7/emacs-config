;;;; Pretty suscripts support
(require 'magic-latex-buffer)

(setq magic-latex-ignored-properties
      '(font-lock-comment-face
        font-lock-comment-delimiter-face
        font-lock-string-face))

(defun suscript-jit-prettifier (beg end) ; redefined ml/jit-prettifier
  (goto-char beg)
  (ml/remove-pretty-overlays beg end)
  ;; prettify suscripts
  (save-excursion
    (while (ignore-errors (ml/search-suscript t end))
      (let* ((body-beg (match-beginning 1))
             (body-end (match-end 1))
             (delim-beg (match-beginning 0))
             (delim-end (match-end 0))
             ;; the point can be already prettified in a recursive
             ;; suscript like "a_{b_c}".
             (oldov (ml/overlay-at body-beg 'category 'ml/ov-pretty))
             (oldprop (and oldov (overlay-get oldov 'display)))
             (priority-base (and oldov (or (overlay-get oldov 'priority) 0)))
             (raise-base (or (cadr (assoc 'raise oldprop)) 0.0))
             (height-base (or (cadr (assoc 'height oldprop)) 1.0))
             (ov1 (ml/make-pretty-overlay delim-beg delim-end 'invisible t))
             ;; new overlay must have higher priority than the old
             ;; one.
             (ov2 (ml/make-pretty-overlay
                   body-beg body-end 'priority
                   (when oldov (1+ priority-base)))))
        (cl-case (string-to-char (match-string 0))
          ((?_) (overlay-put
                ov2 'display
                `((raise ,(- raise-base 0.2))
                  (height ,(* height-base 0.8)))))
          ((?^) (overlay-put
                ov2 'display
                `((raise ,(+ raise-base 0.2))
                  (height ,(* height-base 0.8))))))))))

(define-minor-mode magic-suscript-buffer
    "Redefinition of the magic-latex-buffer mode, that doesn't conflict with lisp-mode."
  :init-value nil
  :global nil
  :lighter " suscript"
  (if magic-suscript-buffer
      (progn
        (jit-lock-mode 1)
        (setq-local font-lock-multiline t)
        (jit-lock-register 'suscript-jit-prettifier)
        (jit-lock-register 'font-lock-fontify-region))
      (jit-lock-unregister 'suscript-jit-prettifier)
      (jit-lock-unregister 'font-lock-fontify-region)
      (ml/remove-pretty-overlays (point-min) (point-max))
      (font-lock-refresh-defaults)))

(add-hook 'lisp-mode-hook 'magic-suscript-buffer)
(add-hook 'emacs-lisp-mode-hook 'magic-suscript-buffer)

(add-hook 'latex-mode-hook 'magic-latex-buffer)
