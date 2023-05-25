;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package vundo
  :bind
  ("C-c u" . 'vundo)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols)
  )

(use-package undo-fu
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu)
    (evil-set-undo-system 'undo-fu))
  :bind
  ("C-c z" .  'undo-fu-only-undo)
  ("C-c Z" . 'undo-fu-only-redo))

;; (use-package undo-fu-session
;;   :after undo-fu
;;   :config
;;   (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
;;   :bind
;;   (global-undo-fu-session-mode 1))

(provide 'p-undo)
