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

(use-package helm
  :init
  (require 'helm-config)
  :bind (("M-x" . helm-M-x)
	 ("C-:" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" .  helm-browse-project))
  :config
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action))
  (helm-migemo-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t))
  


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
  :commands global-auto-complete-mode
  :init
  (global-auto-complete-mode t)
  :config
  (bind-keys :map ac-completing-map
             ("C-n" . ac-next)
             ("C-p" . ac-previous))
  (ac-config-default))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        (concat user-emacs-directory "snippets") ;;作成するスニペットはここに入る
        ))

(use-package iedit)

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package migemo
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  ;; Set your installed path
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
        
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode +1)
  (custom-set-variables
   '(ace-isearch-jump-delay 0.5)))

;;;elpy.el
(use-package elpy
  :config
  (elpy-enable)
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))
  )

(use-package projectile
  :commands (projectile-mode projectile-rails-on)
  :init
  (add-hook 'ruby-mode-hook 'projectile-mode)
  (add-hook 'ruby-mode-hook 'projectile-rails-on)  
  :config
  (setq projectile-switch-project-action 'projectile-dired))

(use-package helm-projectile
  :defer t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-rails-expand-snippet nil))

(use-package magit
  :defer t
 ;; :config
 ;; (set-face-background 'magit-section-highlight "DeepSkyBlue") 
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-jump-delay 0.5)
 '(flymake-gui-warnings-enabled nil)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-
                              (setq)
                              ource-recentf helm-source-buffer-not-found helm-quickrun-source)))
 '(robe-completing-read-func (quote helm-robe-completing-read)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "black" :distant-foreground "white"))))
 '(magit-section-highlight ((t (:background "keyboardFocusIndicatorColor")))))
(set-face-foreground 'highlight nil)
