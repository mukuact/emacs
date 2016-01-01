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
(require 'pallet)
(pallet-mode t)
(require 'use-package)

;;;key-binding
;; C-mにnweline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;;"C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
;;C-h を　backspace に置き換える
 (keyboard-translate ?\C-h ?\C-?)
;; スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))

;;;System configuration
;;行番号を表示
(setq inhibit-startup-message t)
;;初期画面非表示
(global-linum-mode t)
;;ツールバー非表示
(tool-bar-mode -1)
;;タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 背景を透過に設定
(set-frame-parameter nil 'alpha 95)
;;リージョンの背景色を変更
;;(set-face-background 'region "seagreen4")
(global-hl-line-mode nil)
;; paren-moden
(setq show-paren-delay 0);表示までの秒数。初期値は０．１２５
(show-paren-mode t);有効化
;;カッコのスタイル：expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;;フェイスを変更する
(load-theme 'misterioso t)

;; backup の保存先
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/auto-save-list"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "~/.emacs.d/auto-save-list/") t)))

;;;elisp setting
(use-package helm-config
  :bind (("M-x" . helm-M-x)
	 ("C-:" . helm-for-files))
