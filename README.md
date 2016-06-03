# emacs-config
My Emacs configuration code (and dirty hacks)

Custom fonts "Anonymous Pro", "PragmataPro" and "Fira Code Medium" are used,
they must be installed in the system in order to use this code.

------------------------------------------

Files:
* init.el                        - main script that loads everything, should be loaded from .emacs
* init-emacs.el                  - generic Emacs customizations, installation of Emacs packages
* init-extensions.el             - Emacs packages loading & tuning code
* init-cl-symbols-list.el        - list of CL standard symbols for syntax highlighting purposes
* init-keys.el                   - binding of common key sequences
* init-buffer-keys.el            - binding of keys to work with buffer configuration
* init-lisp-mode-keys.el         - binding of Lisp modes only key sequences

* elisp/slime-repl-ansi-color.el - SLIME REPL support for ANSI colored output
* elisp/random-idle-quote.el     - random-idle-quote mode
* color-themes/granger-theme.el  - Emacs color theme

* ivanp7.lisp                    - Common Lisp on-startup setup

* keys-description-ru.org        - description of all defined keys in russian
* screenshot*.png                - example views: the result all this magic creates
