;;;; Prettify symbols (pretty-mode)
(require 'pretty-mode)

(global-pretty-mode -1)
(add-hook 'lisp-mode-hook 'pretty-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-mode)

;; Disabling pretty-mode in *slime-macroexpansion* buffer because of bug
;; (defun slime-create-macroexpansion-buffer-advice (func)
;;   (let ((new-buffer (funcall func)))
;;     (when new-buffer
;;       (pop-to-buffer new-buffer)
;;       (pretty-mode -1)
;;       new-buffer)))

;; (advice-add 'slime-create-macroexpansion-buffer
;;             :around 'slime-create-macroexpansion-buffer-advice)

;; Disabling case inconsistency because of bug
(setq-default font-lock-keywords-case-fold-search nil)
(add-hook 'lisp-mode-hook
          (lambda () (setq font-lock-keywords-case-fold-search nil)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq font-lock-keywords-case-fold-search nil)))

;; redefining pretty-patterns:
;; these patterns would be replaced only if separate symbols
;; (neither in comments nor strings)
(defun pretty-patterns ()
  "*List of pretty patterns.
 
Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)"
  (let* ((lispy '(scheme emacs-lisp lisp clojure jess clips))
         (mley '(haskell tuareg sml fsharp))
         (c-like '(c c++ perl sh python java ess ruby javascript coffee groovy))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       ;;; Values taken directly from
       ;;; `The Unicode Standard, Version 5.2' documented
       (?\u00AC :neg (:logic) (:not "not" ,@lispy))
       ;; duplucation to get around of font-lock bug
       (?\u00AC :neg-up (:logic) (:not-up "NOT" ,@lispy))
       (?\u00AC :neg-up2 (:logic) (:not-up2 "Not" ,@lispy))
       
       (?\u2227 :wedge (:logic) (:and "and" ,@lispy))
       (?\u2227 :wedge-up (:logic) (:and-up "AND" ,@lispy))
       (?\u2227 :wedge-up2 (:logic) (:and-up2 "And" ,@lispy))
       
       (?\u2228 :vee (:logic) (:or "or" ,@lispy))
       (?\u2228 :vee-up (:logic) (:or-up "OR" ,@lispy))
       (?\u2228 :vee-up2 (:logic) (:or-up2 "Or" ,@lispy))
       
       (?\u221A :sqrt (:arithmetic) (:sqrt "sqrt" ,@lispy))
       (?\u221A :sqrt-up (:arithmetic) (:sqrt-up "SQRT" ,@lispy))
       (?\u221A :sqrt-up2 (:arithmetic) (:sqrt-up2 "Sqrt" ,@lispy))
       
       (?\u005E :expt (:arithmetic) (:expt "expt" ,@lispy))
       (?\u005E :expt-up (:arithmetic) (:expt-up "EXPT" ,@lispy))
       (?\u005E :expt-up2 (:arithmetic) (:expt-up2 "Expt" ,@lispy))
       
       ;;(?\u0025 :mod (:arithmetic) (:mod "mod" ,@lispy))
       ;;(?\u0025 :mod-up (:arithmetic) (:mod-up "MOD" ,@lispy))
       
       (?\u00D7 :mult (:arithmetic) (:mult "*" ,@lispy))
       
       ;;(?\u00F7 :div (:arithmetic) (:div "/" ,@lispy)) ; conflicts with /=
       ;;(?\u2116 :nth (:sets) (:nth "nth" ,@lispy))
       ;;(?\u2116 :nth-up (:sets) (:nth-up "NTH" ,@lispy))
       
       (?\u2208 :member (:sets) (:member "member" ,@lispy))
       (?\u2208 :member-up (:sets) (:member-up "MEMBER" ,@lispy))
       (?\u2208 :member-up2 (:sets) (:member-up2 "Member" ,@lispy))
       
       (?\u2200 :every (:sets) (:every "every" ,@lispy))
       (?\u2200 :every-up (:sets) (:every-up "EVERY" ,@lispy))
       (?\u2200 :every-up2 (:sets) (:every-up2 "Every" ,@lispy))
       
       (?\u2203 :some (:sets) (:some "some" ,@lispy))
       (?\u2203 :some-up (:sets) (:some-up "SOME" ,@lispy))
       (?\u2203 :some-up2 (:sets) (:some-up2 "Some" ,@lispy))
       
       (?\u2204 :notany (:sets) (:notany "notany" ,@lispy))
       (?\u2204 :notany-up (:sets) (:notany-up "NOTANY" ,@lispy))
       (?\u2204 :notany-up2 (:sets) (:notany-up2 "Notany" ,@lispy))
       
       (?\u2190 :setf (:equality) (:setf "setf" ,@lispy))
       (?\u2190 :setf-up (:equality) (:setf-up "SETF" ,@lispy))
       (?\u2190 :setf-up2 (:equality) (:setf-up2 "Setf" ,@lispy))
       
       (?\u21C7 :psetf (:equality) (:setf "psetf" ,@lispy))
       (?\u21C7 :psetf-up (:equality) (:setf-up "PSETF" ,@lispy))
       (?\u21C7 :psetf-up2 (:equality) (:setf-up2 "Psetf" ,@lispy))
       ))))
