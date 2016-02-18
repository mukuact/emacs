; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'right)
(setq popwin:popup-window-width '76)
(setq popwin:adjust-other-windows t)

;; undo-tree
(push '(" *undo-tree*" :width 0.2 :position right) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; sdic
(push '("*sdic*") popwin:special-display-config)

;; Completions
(push '("*Completions*") popwin:special-display-config)

;; HELM
(push '("^\*helm .+\*$" :width 70 :regexp t :position right) popwin:special-display-config)

