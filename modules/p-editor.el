;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package ace-window
  :bind (("C-x o" . 'ace-window)))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward)
         ("C-c d" . hungry-delete-forward)))
;;(global-hungry-delete-mode)

(use-package crux)

(global-set-key [remap kill-line] #'crux-kill-and-join-forward)

(use-package multiple-cursors
  :bind
  (("C-S-<mouse-1>" . mc/toggle-cursor-on-click)
   ("C-c n a" . mc/mark-all-like-this)
   ("C-c n n" . mc/mark-next-like-this)
   ("C-c n p" . mc/mark-previous-like-this)
   ))

(use-package iedit)

;;; tiny 序号宏展开
(use-package tiny
  ;; 可选绑定快捷键，笔者个人感觉不绑定快捷键也无妨
  :bind
  ("C-;" . tiny-expand))

;;; highlight-symbol
(use-package highlight-symbol
  :init (highlight-symbol-mode)
  :bind ("C-c w" . highlight-symbol)) ;; 按下 F3 键就可高亮当前符号

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :init (rainbow-delimiters-mode)
)


(with-eval-after-load 'rainbow-delimiters
  ;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
)

(use-package rainbow-mode
  :config
  (rainbow-mode t)
  )

(use-package change-inner
  :config
  (global-set-key (kbd "C-c v i") 'change-inner)
  (global-set-key (kbd "C-c v o") 'change-outer))



(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package vlf
  :after minemacs-loaded
  :demand t)


(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Pulse for evil commands
  (goggles-define undo primitive-undo evil-undo)
  (goggles-define yank yank yank-pop evil-yank evil-yank-line)
  (goggles-define kill kill-region)
  (goggles-define delete delete-region evil-delete evil-delete-line))

(provide 'p-editor)
