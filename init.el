;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "modules")))

;; Core
(require 'init-const)
(require 'init-startup)
(require 'init-better-defaults)
(require 'init-performance)
(require 'init-binds)
(require 'init-keymaps)
;;(require 'init-keymaps)
(require 'init-elpa)
;;(require 'init-ws-mode)

;; Personal
(require 'p-theme)
(require 'p-cmp)
(require 'p-editor)
(require 'p-undo)
(require 'p-files)
