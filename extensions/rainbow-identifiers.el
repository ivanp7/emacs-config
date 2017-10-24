;;;; Rainbow identifiers
(require 'rainbow-identifiers)

(setq rainbow-identifiers-cie-l*a*b*-lightness 65)
(setq rainbow-identifiers-cie-l*a*b*-saturation 100)
(setq rainbow-identifiers-cie-l*a*b*-color-count 32)

(setq use-colors-count 28)
(setq use-colors-shift -4)

(setq rainbow-identifiers-face-count
      rainbow-identifiers-cie-l*a*b*-color-count)

(defvar rainbow-identifiers-default-color "burlywood")

(setq rainbow-identifiers-choose-face-function
      (lambda (hash)
        (if (numberp hash)
            (rainbow-identifiers-cie-l*a*b*-choose-face
             ;; hack to get rid of the excess of blue colors
             (* (+ use-colors-shift (mod hash rainbow-identifiers-face-count))
                (/ use-colors-count 1.0
                   rainbow-identifiers-cie-l*a*b*-color-count)))
            (cond
              ((eq hash :cl-special)
               (list (list :foreground "white" :slant 'italic)))
              ((eq hash :cl-standard)
               (list (list :foreground "white")))
              ((eq hash :default)
               (list (list :foreground rainbow-identifiers-default-color)))))))

(defvar cl-special-operators-symbols-list nil)
(defvar cl-symbols-list nil)

(with-temp-buffer
    (insert-file-contents (concat cl-ide-init-aux-path "cl-symbols-list.sexp"))
  (goto-char (point-min))
  (let ((symbols-list (sexp-at-point)))
    (setq cl-special-operators-symbols-list
          (mapcar 'symbol-name (getf symbols-list :special-operators)))
    (setq cl-symbols-list
          (mapcar 'symbol-name (getf symbols-list :all)))))

;; --- Binary tree implementation ------------------------------------------

(defvar rainbow-identifiers-custom-binary-tree nil)

(defun make-binary-tree (entry left right)
  (list entry left right))

(defun tree-entry (tree)
  (first tree))

(defun tree-left-branch (tree)
  (second tree))

(defun tree-right-branch (tree)
  (third tree))

(defun lookup-binary-tree (x tree)
  (cond ((null tree) nil)
        ((string-equal x (car (tree-entry tree))) (tree-entry tree))
        ((string-lessp x (car (tree-entry tree)))
         (lookup-binary-tree x (tree-left-branch tree)))
        (t (lookup-binary-tree x (tree-right-branch tree)))))

(defun adjoin-binary-tree (x tree)
  (cond ((null tree) (make-binary-tree x nil nil))
        ((string-equal (car x) (car (tree-entry tree))) tree)
        ((string-lessp (car x) (car (tree-entry tree)))
         (make-binary-tree
          (tree-entry tree)
          (adjoin-binary-tree x (tree-left-branch tree))
          (tree-right-branch tree)))
        (t (make-binary-tree
            (tree-entry tree)
            (tree-left-branch tree)
            (adjoin-binary-tree x (tree-right-branch tree))))))

(defun tree->list (tree &optional exclude-p)
  (cl-labels ((copy-to-list
               (tree result-list)
               (if (null tree)
                   result-list
                   (copy-to-list
                    (tree-left-branch tree)
                    (if (or (null exclude-p) (not (funcall
                                                exclude-p
                                                (tree-entry tree))))
                        (cons (tree-entry tree)
                              (copy-to-list (tree-right-branch tree)
                                            result-list))
                        (copy-to-list (tree-right-branch tree)
                                      result-list))))))
             (copy-to-list tree nil)))

(defun partial-tree (elts n)
  (if (zerop n)
      (cons nil elts)
      (let* ((left-size (truncate (/ (1- n) 2)))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (1+ left-size)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-binary-tree this-entry left-tree right-tree)
              remaining-elts))))

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun delete-from-binary-tree (x tree)
  (list->tree (tree->list tree (lambda (entry) (string-equal x (car entry))))))

;; --- End of binary tree implementation ----------------------------------

(defun rainbow-identifiers--identifier-hash (identifier)
  (let* ((hash (secure-hash 'sha1 identifier nil nil t))
         (len (length hash))
         (i (- len rainbow-identifiers--hash-bytes-to-use))
         (result 0))
    (while (< i len)
           (setq result (+ (* result 256) (aref hash i)))
           (setq i (1+ i)))
    result))

(defvar random-color-by-default nil)

(defvar rainbow-identifiers--hash-function-plugin
  (lambda (identifier)
    (if (eq major-mode 'lisp-mode)
        (cond
          ((member identifier cl-special-operators-symbols-list)
           :cl-special)
          ((member identifier cl-symbols-list) :cl-standard)))))

(defun rainbow-identifiers--hash-function (identifier)
  "Redefined version of the standard 'rainbow-identifiers--hash-function'"
  (let* ((identifier (downcase identifier))
         (record (lookup-binary-tree
                  identifier
                  rainbow-identifiers-custom-binary-tree))
         (plugin-res (funcall rainbow-identifiers--hash-function-plugin
                              identifier)))
    (cond
      (record (cdr record))
      (plugin-res plugin-res)
      (t (if random-color-by-default
             (rainbow-identifiers--identifier-hash identifier)
             :default)))))

(defvar rainbow-identifiers-supported-modes '(lisp-mode emacs-lisp-mode))

(defvar rainbow-identifiers-tune-delta 1)
(defun rainbow-identifiers-tune ()
  (interactive)
  (if (member major-mode rainbow-identifiers-supported-modes)
      (progn
        (let ((sym (thing-at-point 'symbol)))
          (if sym
              (let* ((sym (downcase (substring-no-properties sym)))
                     (record (lookup-binary-tree
                              sym
                              rainbow-identifiers-custom-binary-tree)))
                (if record
                    (setf (cdr record)
                       (mod (+ rainbow-identifiers-tune-delta
                               (cdr record))
                            rainbow-identifiers-face-count))
                    (setf rainbow-identifiers-custom-binary-tree
                       (adjoin-binary-tree
                        (cons sym
                              (mod (rainbow-identifiers--identifier-hash sym)
                                   rainbow-identifiers-face-count))
                        rainbow-identifiers-custom-binary-tree))))))
        (font-lock-fontify-buffer))
      (message "Rainbow identifiers tune is not supported in this mode.")))

(defun rainbow-identifiers-cancel-tuning ()
  (interactive)
  (if (member major-mode rainbow-identifiers-supported-modes)
      (progn
        (let ((sym (thing-at-point 'symbol)))
          (if sym
              (let* ((sym (downcase (substring-no-properties sym)))
                     (record (lookup-binary-tree
                              sym
                              rainbow-identifiers-custom-binary-tree)))
                (if record
                    (setf rainbow-identifiers-custom-binary-tree
                       (delete-from-binary-tree
                        (car record)
                        rainbow-identifiers-custom-binary-tree))))))
        (font-lock-fontify-buffer))
      (message "Rainbow identifiers tune is not supported in this mode.")))

(defvar rainbow-identifiers-custom-list nil
  "User-selected custom hash values for specific symbols.")

;; load 'rainbow-identifiers-custom-list' from file
(defun rainbow-identifiers-load-tune ()
  (interactive)
  (setq rainbow-identifiers-custom-list
        (sort (let ((tuning-file (concat cl-ide-init-ext-path
                                         "rainbow-identifiers/tuning.sexp")))
                (when (file-readable-p tuning-file)
                  (with-temp-buffer
                      (insert-file-contents tuning-file)
                    (goto-char (point-min))
                    (sexp-at-point))))
              (lambda (rec1 rec2)
                (string-lessp (car rec1) (car rec2)))))
  (setq rainbow-identifiers-custom-binary-tree
        (list->tree rainbow-identifiers-custom-list))
  (font-lock-fontify-buffer))

;; save 'rainbow-identifiers-custom-list' to file
(defun rainbow-identifiers-save-tune ()
  (interactive)
  (setq rainbow-identifiers-custom-list
        (tree->list rainbow-identifiers-custom-binary-tree))
  (write-region
   (format "(%s)"
           (concat (if rainbow-identifiers-custom-list
                       (format "%s" (car rainbow-identifiers-custom-list))
                       "")
                   (apply 'concat
                          (mapcar (lambda (c) (format "\n %s" c))
                                  (cdr rainbow-identifiers-custom-list)))))
   nil (concat cl-ide-init-ext-path
               "rainbow-identifiers/tuning.sexp")))

;; Customized filter: don't mark numbers, CL notation '#\name',
;; and '@' in ',@', mark '|name|' symbols
(defun rainbow-identifiers-filter (beg end)
  (let ((str (buffer-substring-no-properties beg end)) (len (- end beg))
        (prefix2 (buffer-substring-no-properties
                  (max (point-min) (- beg 2)) beg))
        (prefix11 (buffer-substring-no-properties
                   (max (point-min) (- beg 11)) beg))
        (prefix17 (buffer-substring-no-properties
                   (max (point-min) (- beg 17)) beg))
        (prev-char (char-before beg)) (first-char (char-after beg))
        (second-char (char-after (+ beg 1)))
        (third-char (char-after (+ beg 2)))
        (prev-last-char (char-before (- end 1)))
        (last-char (char-before end)) (next-char (char-after end)))
    (cond
      ((and (equal prev-char ?\|) (equal next-char ?\|)) t)
      ((or (and (= len 1) (equal first-char ?\.))
           ;; (and (equal first-char ?\@) (equal prev-char ?\,))
           (equal prefix2 "#\\") (equal prev-char ?\#)
           (and (or (equal (upcase prefix11) "#<FUNCTION ")
                    (equal (upcase prefix17) "#<STANDARD-CLASS "))
                (equal last-char ?\>))
           (and (equal first-char ?\{)
                (equal prev-last-char ?\})
                (equal last-char ?\>))
           (member first-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
           (and (>= len 2) (member first-char '(?+ ?- ?\.))
                (member second-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
           (and (>= len 3) (member first-char '(?+ ?-)) (equal second-char ?\.)
                (member third-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))) nil)
      (t t))))

(add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)

;; Filter: don't mark identifiers inside comments or strings
(setq rainbow-identifiers-faces-to-override
      '(font-lock-type-face
        font-lock-variable-name-face
        font-lock-constant-face
        font-lock-function-name-face
        font-lock-keyword-face
        font-lock-builtin-face))

;; Installing hooks
(add-hook 'lisp-mode-hook 'rainbow-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)

(add-hook 'window-setup-hook 'rainbow-identifiers-load-tune)
