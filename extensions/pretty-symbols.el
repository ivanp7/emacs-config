;;;; Prettify symbols (pretty-symbols)
(require 'pretty-symbols)
(setq pretty-symbol-categories '(custom)) ; '(lambda relational logical nil)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
;;(add-hook 'lisp-interaction-mode 'pretty-symbols-mode)

;;; Adding custom symbols to the mode:
;; these patterns would be replaced everywhere
(setq pretty-symbol-patterns
      (let ((lisps '(emacs-lisp-mode inferior-lisp-mode inferior-emacs-lisp-mode
                     lisp-mode scheme-mode)))
        (append pretty-symbol-patterns
                `((?\u2205 custom "nil" (,@lisps))
                  ;; uppercase duplication to get around of a bug
                  (?\u2205 custom "NIL" (,@lisps))
                  (?\u223E custom "~" (,@lisps))
                  (?\u2260 custom "/=" (,@lisps))
                  ;;(?\u2248 custom "~=" (,@lisps))
                  (?\u2265 custom ">=" (,@lisps))
                  (?\u2264 custom "<=" (,@lisps))
                  (?\u2192 custom "->" (,@lisps))
                  (?\u2190 custom "<-" (,@lisps))
                  (?\u00B1 custom "\\+-" (,@lisps))
                  (?\u2213 custom "-\\+" (,@lisps))
                  (?\u221E custom "\\<inf\\>" (,@lisps))
                  ;; uppercase duplication to get around of a bug
                  (?\u221E custom "\\<INF\\>" (,@lisps)))
                (loop for pair in
                     (cl-pairlis
                      '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                        "theta" "iota" "kappa" "lambda" "mu" "nu" "xi"
                        "omicron" "pi" "rho" "sigmaf" "sigma" "tau"
                        "upsilon" "phi" "chi" "psi" "omega")
                      (mapcar
                       (lambda (x) (make-char 'greek-iso8859-7 x))
                       (number-sequence 97 121)))
                   collect (list (cdr pair) 'custom
                                 (concat "\\<" (car pair) "\\>") lisps)))))

;; Better fonts for special symbols
;; (defun custom-fonts-for-pretty-symbols ()
;;   (let ((symbols-fonts
;;          '(("PragmataPro" ; monospaced
;;             (#x2208 #x2200 #x2203 #x2204 #x2227 #x2228 #x2213
;;              ;; member every some notany and or minus-plus
;;              #x2205 #x2260
;;              ;; nil /=
;;              #x2248 #x2264 #x2265 #x221A
;;              ;; ~= <= >= sqrt
;;              ))
;;            ("Fira Code Medium" ; not monospaced, but good-looking
;;             (#x2192 #x2190 #x221E)
;;             ;; -> <- inf
;;             ))))
;;     (dolist (font-set symbols-fonts)
;;       (dolist (sym (cadr font-set))
;;         (set-fontset-font "fontset-default" sym (car font-set) nil nil)
;;          ;; -set) nil 'prepend)
;;         ))))

;; (defun custom-fonts-for-pretty-symbols ()
;;   (let ((symbols-fonts
;;          '(("Monospace" ; for Ubuntu
;;             (#x2208 #x2200 (#x2203 . #x2204) #x2227 #x2228 #x2213))
;;            ("Hasklig Medium" ;"DejaVu Sans Mono" ; for Windows
;;             (#x2205 #x2260 #x221E ; #x2208 #x2200 (#x2203 . #x2204)
;;              ))
;;            ("Hasklig Medium" ;"Consolas" ; for Windows
;;             (#x2248 (#x2264 . #x2265) #x221A #x2192 #x2190 ; #x2116
;;              )))))
;;     (dolist (font-set symbols-fonts)
;;       (dolist (sym (cadr font-set))
;;         (set-fontset-font "fontset-default" sym (car font-set) nil nil)
;;          ;; -set) nil 'prepend)
;;         ))))

;;(custom-fonts-for-pretty-symbols)
