;; Rainbow delimiters
(require 'rainbow-delimiters)

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "white"
                    :background "dim gray"
                    :slant 'normal :weight 'normal)
(set-face-attribute 'rainbow-delimiters-mismatched-face nil
                    :foreground "black"
                    :background "dim gray"
                    :slant 'normal :weight 'normal)

(defvar rainbow-delimiters-foreground-colors
  ["white" "gray" "red" "orange" "yellow" "green" "deep sky blue"
           "blue" "magenta"])
(defvar rainbow-delimiters-background-colors
  (make-vector rainbow-delimiters-max-face-count nil))

(defvar rainbow-delimiters-slants
  (make-vector rainbow-delimiters-max-face-count 'normal))

(defvar rainbow-delimiters-weights
  (concatenate 'vector
               [ultra-bold]
               (make-vector (1- rainbow-delimiters-max-face-count) 'normal)))

(dotimes (i 9)
  (set-face-attribute (intern (concat "rainbow-delimiters-depth-"
                                      (number-to-string (1+ i))
                                      "-face"))
                      nil
                      :foreground (elt rainbow-delimiters-foreground-colors i)
                      :background (elt rainbow-delimiters-background-colors i)
                      :slant (elt rainbow-delimiters-slants i)
                      :weight (elt rainbow-delimiters-weights i)))
