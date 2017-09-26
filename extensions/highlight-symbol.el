;;;; Highlight symbol
(require 'highlight-symbol)
(require 'cl-lib)

(setq highlight-symbol-highlight-single-occurrence nil)
(setq highlight-symbol-on-navigation-p t)
(setq highlight-symbol-idle-delay 0.2)
(let ((hook (lambda () (interactive)
               (cl-pushnew '(highlight-symbol-face :underline t)
                           face-remapping-alist :test 'equal)
               (highlight-symbol-mode))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook))

(let ((redraw-fn (lambda (&rest args)
                   (refresh-window))))
  (advice-add 'highlight-symbol-next :after redraw-fn)
  (advice-add 'highlight-symbol-prev :after redraw-fn)
  (advice-add 'query-replace :after redraw-fn)
  (advice-add 'query-replace-regexp :after redraw-fn))

(modify-syntax-entry ?@ "'" lisp-mode-syntax-table)

;; (defun thing-at-point-advice (oldfun &rest args)
;;   (let* ((result (apply oldfun args))
;;          (comma+at-sign-detected)
;;          (new-result (if (and (eql (first args) 'sexp)
;;                             (setf at-sign-detected (eql (elt result 0) ?@)))
;;                          (substring result 1)
;;                          result)))
;;     (if (and at-sign-detected (eql (char-after (point)) ?@))
;;         nil
;;         new-result)))

;; (advice-add 'thing-at-point :around 'thing-at-point-advice)

;; (defun bounds-of-thing-at-point-advice (oldfun &rest args)
;;   (let ((result (apply oldfun args)))
;;     (if result
;;         (let ((prefix-shift
;;                (if (eql (first args) 'sexp)
;;                    (if (eql (char-before (car result)) ?@)
;;                        (if (eql (char-before (1- (car result))) ?,)
;;                            -2 -1)
;;                        0)
;;                    0)))
;;           (cons (+ prefix-shift (car result)) (cdr result))))))

;; (advice-add 'bounds-of-thing-at-point :around 'bounds-of-thing-at-point-advice)
