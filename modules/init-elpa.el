;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA" . 5)))

(setq package-enable-at-startup nil)
(package-initialize)

;; 刷新软件源索引
(unless package-archive-contents
    (package-refresh-contents))

;; 第一个扩展插件：use-package，用来批量统一管理软件包
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; `use-package-always-ensure' 避免每个软件包都需要加 ":ensure t"
;; `use-package-always-defer' 避免每个软件包都需要加 ":defer t"
(setq use-package-always-ensure t
      ;;use-package-always-defer t
      use-package-always-demand t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(require 'use-package)


;; (add-to-list 'package-archives
;;              (cons "melpa" "https://melpa.org/packages/")
;;              t)

;; (custom-set-variables
;;  '(package-selected-packages '(borg)))

;; (if (require 'borg-elpa nil t)
;;     (borg-elpa-initialize)
;;   (package-initialize))



(provide 'init-elpa)
