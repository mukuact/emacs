; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ---------------------------------------------------
;; ファイル操作(dired)関連の設定
;; ---------------------------------------------------
(require 'dired)
(add-hook 'dired-load-hook (lambda () 
                             (load "dired-x")))

;; スペースでマークする (FD like)
(define-key dired-mode-map " " 'dired-toggle-mark)
(defun dired-toggle-mark (arg)
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    (dired-previous-line 1)))

;; 再帰的なコピー/削除を行う
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)


;; 2画面ファイラーモード
;; 画面分割で2つのDiredを同時に起動している場合、
;; コピーや移動先のデフォルトが、もう片方のDiredで開いているディレクトリになる
(setq dired-dwim-target t)

;;diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; ---------------------------------------------------
;; ファイルの一括操作
;; ---------------------------------------------------

;; dired で r を押して編集開始。
;; C-x C-s で確定、C-c C-k で破棄
;;    ESC l:ファイル名を小文字にする
;;    ESC c:ファイル名の一文字を大文字にする
;;    ESC u:ファイル名を大文字にする
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)



;; ---------------------------------------------------
;; 表示順の設定
;; ---------------------------------------------------
(require 'ls-lisp)

;; ディレクトリとファイルを分けて表示
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; ファイル名の大文字小文字無視でソート
(setq ls-lisp-ignore-case t)

;; 表示項目と内容の設定(lsコマンドのオプションとほぼ同一)
(setq dired-listing-switches "-AFlGh")



;; ---------------------------------------------------
;; W でOS関連付けプログラムでファイルを開く
;; E でエクスプローラーでフォルダを開く
;; http://www49.atwiki.jp/ntemacs/pages/19.html
;; ---------------------------------------------------

;; OSタイプ を調べる function
(defun os-type ()
  (let ((os-type (shell-command-to-string "uname")))
    (cond ((string-match "CYGWIN" os-type)
           "win")
          ((string-match "Linux" os-type)
           "linux")
          ((string-match "Darwin" os-type)
           "mac"))))

;; OS でファイル、ディレクトリ、URL を直接開くためのコマンドを決定する function
(defun os-open-command-name ()
  (let ((os-type (os-type)))
    (if os-type
        (let ((command-name-list
               (cond ((string= "win" os-type)
                      '("cygstart"))
                     ((string= "linux" os-type)
                      '("xdg-open"))
                     ((string= "mac" os-type)
                      '("open")))))
          (dolist (command-name command-name-list)
            (if (not (string=  (shell-command-to-string
                                (concat "which " command-name " 2> /dev/null"))
                               ""))
                (return command-name)))))))

;; OS で直接、ファイル、ディレクトリ、URL を開く command
(defun os-open-command (filename)
  (interactive)
  (let* ((default-directory (if (file-regular-p filename)
                                (file-name-directory filename)
                              default-directory))
         (localname (if (file-remote-p filename)
                        (tramp-file-name-localname
                         (tramp-dissect-file-name filename))
                      filename))
         (os-open-command-name (os-open-command-name)))
    (when os-open-command-name
      (cond ((and (string= os-open-command-name "xdg-open")
                  (not (file-remote-p default-directory)))
             ;; 以下の URL の対策を行う
             ;; http://d.hatena.ne.jp/mooz/20100915/p1
             ;; http://i-yt.info/?date=20090829#p01
             (let (process-connection-type)
               (start-process "os-open-start" nil os-open-command-name localname)))
            (t
             (shell-command-to-string (concat os-open-command-name " "
                                              (shell-quote-argument localname) " &"))))
      (message "%s" (concat os-open-command-name " " localname)))))

;; dired で W 押下時に、カーソル位置のファイルを OS で直接起動する
(define-key dired-mode-map (kbd "W")
  (lambda ()
    (interactive)
    (os-open-command (dired-get-filename nil t))))

;; dired で E 押下時に、開いているディレクトリを OS で直接開く
(define-key dired-mode-map (kbd "E")
  (lambda ()
    (interactive)
    (os-open-command (dired-current-directory))))



;; ---------------------------------------------------
;; フォルダ移動時にバッファを生成しない
;; ---------------------------------------------------
(defun dired-my-advertised-find-file ()
  (interactive)
  (let ((kill-target (current-buffer))
        (check-file (dired-get-filename)))
    (funcall 'dired-advertised-find-file)
    (if (file-directory-p check-file)
        (kill-buffer kill-target))))

(defun dired-my-up-directory (&optional other-window)
  "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (if other-window
              (dired-other-window up)
            (progn
              (kill-buffer (current-buffer))
              (dired up))
          (dired-goto-file dir))))))

(define-key dired-mode-map "\C-m" 'dired-my-advertised-find-file)
(define-key dired-mode-map "^" 'dired-my-up-directory)
