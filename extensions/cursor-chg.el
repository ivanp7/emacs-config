;;;; Change cursor
(require 'cursor-chg)  ; Load the library
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode

(setq curchg-default-cursor-color "white")
(setq curchg-input-method-cursor-color "orange")
(setq curchg-default-cursor-type 'bar)
(setq curchg-idle-cursor-type 'box)
(setq curchg-overwrite/read-only-cursor-type 'hbar)

(setq cursor-in-non-selected-windows 'hollow)
(setq-default cursor-in-non-selected-windows 'hollow)

(curchg-change-cursor-when-idle-interval 10)
