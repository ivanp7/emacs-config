;;;; Undo tree
(require 'undo-tree)

(define-global-minor-mode global-undo-tree-mode
    undo-tree-mode
  (lambda () (undo-tree-mode t)))

(global-undo-tree-mode)
