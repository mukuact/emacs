; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-OA

;; load-path へのサブディレクトリの一括登録関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;;Cask (package manager)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(use-package init-loader
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load  "~/.emacs.d/inits"))



;; ---------------------------------------------------
;; global key-bindings
;; ---------------------------------------------------

;; C-mにnweline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;;"C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
;;C-h を　backspace に置き換える
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
(global-set-key [f1] 'help-for-help)
;; スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))


;; ---------------------------------------------------
;; elisp setting
;; ---------------------------------------------------
(use-package pallet
  :config
  (pallet-mode t))

(use-package helm-config
  :bind (("M-x" . helm-M-x)
	 ("C-:" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c i" . helm-imenu)))


(use-package helm-gtags
  :commands helm-gtags-mode
  :init
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'helm-gtags-mode-hook
            '(lambda ()
               (local-set-key (kbd "M-g .") 'helm-gtags-find-tag)
               (local-set-key (kbd "M-g r") 'helm-gtags-find-rtag)
               (local-set-key (kbd "M-g s") 'helm-gtags-find-symbol)
               (local-set-key (kbd "<M-left>") 'helm-gtags-pop-stack))))

(use-package undo-hist
  :config
  (undohist-initialize))

(use-package undo-tree
  :bind (("C-/" . undo-tree-undo)
	 ("C-?" . undo-tree-redo)))

(use-package elscreen
  :config
  (elscreen-start)
  (setq elscreen-display-tab nil))

(use-package auto-complete-config
  :config
  (bind-keys :map ac-completing-map
             ("C-n" . ac-next)
             ("C-p" . ac-previous))
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package iedit)

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode +1)
  (custom-set-variables
   '(ace-isearch-jump-delay 0.5)))

;;;elpy.el
(use-package elpy
  :config
  (elpy-enable))

(use-package projectile
  :commands (projectile-mode projectile-rails-on)
  :init
  (add-hook 'ruby-mode-hook 'projectile-mode)
  (add-hook 'ruby-mode-hook 'projectile-rails-on)  
  :config
  (setq projectile-switch-project-action 'projectile-dired))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-rails-expand-snippet nil))


