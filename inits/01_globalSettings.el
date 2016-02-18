;; ---------------------------------------------------
;; System configuration
;; ---------------------------------------------------
;;beep音抑制
(setq ring-bell-function 'ignore)
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
;;(setq show-paren-delay 0);表示までの秒数。初期値は０．１２５
(show-paren-mode t);;有効化
;;カッコのスタイル：expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;;フェイスを変更する
(load-theme 'tango-dark t)
(set-cursor-color 'red)
;; カーソル上の関数名を表示する
(which-func-mode t)
;; 行番号の表示
(line-number-mode nil)
;; インデントをスペースに統一
(setq-default indent-tabs-mode nil)
;; C-x C-f での意味の無いパス表示をグレーアウトする
(file-name-shadow-mode t)
;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)
;;; cursol
;; カーソル点滅表示
(blink-cursor-mode 0)
;;;region
;: バッファ切り替え時にリージョンを保持
(setq highlight-nonselected-windows t)
;;magit color-setting
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-section-highlight ((t (:background "keyboardFocusIndicatorColor")))))
;;;default package
(electric-pair-mode)



;; backup の保存先
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/auto-save-list"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "~/.emacs.d/auto-save-list/") t)))

;;;---------------------------------------------------
;;; 初期のウインドウサイズの設定
;;; ---------------------------------------------------

(setq initial-frame-alist
      (append
       '((top    . 0)                 ; フレームの縦位置(ドット数)
         (left   . 590)               ; フレームの横位置(ドット数)
         (width  . 120)               ; フレーム幅(文字数)
         (height . 51)                ; フレーム高(文字数)
         (alpha . (nil nil nil nil))) ; フレームの透明度 透過率を指定 nilで0%
       initial-frame-alist))




