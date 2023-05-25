;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package treemacs
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-c t f" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-all-the-icons
  :after treemacs all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  :bind
  ("C-c a" . 'dirvish)
  )

(provide 'p-files)
