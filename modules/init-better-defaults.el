;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Dark theme.
;; (load-theme 'wombat)
;; (set-face-background 'default "#111")

(global-eldoc-mode 1) ; eglot uses this too

(electric-pair-mode -1) ; auto parens in pairs

(add-hook 'before-save-hook 'whitespace-cleanup) ; auto strip whitespace

(delete-selection-mode) ; allow highlight and backspace over text like a normal editor

;; clean look
(setq inhibit-startup-screen t
      initial-major-mode 'emacs-lisp-mode
      load-prefer-newer t)

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default
 frame-resize-pixelwise t ; support better certain window managers like ratpoison

 ;; these settings still should be set on a per language basis, this is just a general default
 indent-tabs-mode nil ; spaces > tabs
 tab-width 8 ; tab is 8 spaces
 fill-column 79 ; python friendly

 ;; better security
 gnutls-verify-error t
 gnutls-min-prime-bits 2048

 ;; dont expire a passphrase
 password-cache-expiry nil

 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t
 ediff-window-setup-function 'ediff-setup-windows-plain

 ;; the most reliable tramp setup I have found (used at work every day...)
 tramp-default-method "ssh"
 tramp-copy-size-limit nil
 tramp-use-ssh-controlmaster-options nil

 ;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
 ;; Host *
 ;; ForwardAgent yes
 ;; AddKeysToAgent yes
 ;; ControlMaster auto
 ;; ControlPath ~/.ssh/master-%r@%h:%p
 ;; ControlPersist yes
 ;; ServerAliveInterval 10
 ;; ServerAliveCountMax 10

 vc-follow-symlinks t ; open symlinks, don't ask confusing questions

 ring-bell-function 'ignore ; be quiet

 browse-url-browser-function 'eww-browse-url ; use a text browser --great for clicking documentation links
 )


(defvar BACKDIR (expand-file-name "~/.emacs_backup/"))
;; override auto-save-file-name-transforms
;; (setq auto-save-file-name-transforms
;;   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat BACKDIR "\\2") t)))
;; or, you could prepend instead!
(setq auto-save-file-name-transforms
  (cons `(,(car (car auto-save-file-name-transforms))
          ,(concat BACKDIR "\\2") t) auto-save-file-name-transforms))

(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name "auto-save-list/.saves-" BACKDIR)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

;; BACKUP
(setq backup-directory-alist       ; File name pattxerns and backup directory names.
      `(("." . ,(expand-file-name "~/.emacs_backup")))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files t       ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

(setq recentf-save-file "~/.emacs_backup/recentf")
(setq user-emacs-directory "~/.emacs_backup/var")

;; Recent files
(require 'recentf)
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100
      recentf-exclude '("/home/louis/Documents/Mail.+"
                        "/home/louis/Documents/Notes.+"
                        ))


;; 使用 mouse-avoidance-mode 可以精细地控制鼠标行为。
(setq-default mouse-yank-at-point t) ; Yank at point rather than pointer
(mouse-avoidance-mode 'exile)        ; Avoid collision of mouse with point

;; 鼠标在 tty 模式下处于活动状态。
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") #'scroll-up-line))

;; 滚动更流畅。
(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
              scroll-margin 2                 ; Add a margin when scrolling vertically
              recenter-positions '(5 bottom)) ; Set re-centering positions

;; 允许系统和 Emacs 剪贴板顺利通信（双向）
(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

(defalias 'yes-or-no-p 'y-or-n-p) ; don't make us spell "yes" or "no"


(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-forget-unreadable-files t)
(save-place-mode 1)



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))



(provide 'init-better-defaults)
