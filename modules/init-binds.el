;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; UNBIND ANNOYANCES

;; (global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-m"))

(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c C-t") 'toggle-transparency)

;; TAB AUTO COMPLETION
(setq tab-always-indent 'complete) ; used by eglot for auto-completion as well
;; (setq completion-cycle-threshold nil) ; Always show all candidates in popup menu

;; BETTER-DEFAULTS

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; MATCHING BRACKET LIKE VIM's "%"

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

;; COLLECTION OF REDICULOUSLY USEFUL EXTENSIONS

(global-set-key (kbd "C-c f i") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))

(with-eval-after-load 'crux
  (global-set-key (kbd "C-c k") 'crux-smart-kill-line)
  (global-set-key (kbd "C-c c b") 'crux-kill-line-backwards)
  (global-set-key (kbd "C-c c o") 'crux-smart-open-line)
  (global-set-key (kbd "C-c c l") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c c c") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c c j") 'crux-top-join-line)
  (global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region)
  (global-set-key [remap kill-line] #'crux-kill-and-join-forward))

;; BROWSE KILL RING

(with-eval-after-load 'browse-kill-ring
  (global-set-key (kbd "M-y") 'browse-kill-ring))

;; REGEXP SEARCH

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; FIND FILES

(defalias 'ff 'find-name-dired)

;; GREP FILES

(defalias 'rg 'rgrep)

;; DIFF

(defalias 'ed 'ediff)
(defalias 'edb 'ediff-buffers)

;; PROJECT MGMT

(with-eval-after-load 'projectile
    (defalias 'proj 'projectile-commander)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; GIT

(with-eval-after-load 'magit
  (defalias 'git 'magit))

;; LINTER

(with-eval-after-load 'flymake
  (defun spartan-lint ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))
  (defalias 'lint 'spartan-lint))

;; PASTEBIN

(with-eval-after-load 'webpaste
  (defalias 'paste 'webpaste-paste-buffer-or-region))

;; DUMB TERM

(with-eval-after-load 'better-shell
  (defalias 'sh 'better-shell-for-current-dir))

;; VTERM

(with-eval-after-load 'vterm
  (defalias 'vt 'vterm)
  (global-set-key (kbd "C-c C-v") 'vterm)
  (global-set-key (kbd "C-c C-j") 'vterm-copy-mode)
  (global-set-key (kbd "C-c C-k") 'vterm-copy-mode-done)
  (global-set-key (kbd "C-c v") 'vterm))

;; COMPILE COMMAND

(setq compile-command "make -k ")
(global-set-key (kbd "<f5>") 'compile)

;; EXECUTE SCRIPT

(with-eval-after-load 'spartan-shell
  (global-set-key (kbd "<f6>") 'spartan-script-execute))

;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 6))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 6))
;; 绑定到快捷键
(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)


;;;------------------------------
;; Open files as root
(defun eshell/sudo-open (filename)
  "Open a file as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

;;;------------------------------
;; Super - Control - RET to open eshell
(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

(global-set-key (kbd "C-c o S s") 'eshell-other-window)

;;;------------------------------
;; 重写的 make-frame 创建框架并切换到 *scratch*缓冲
(defun my/make-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

;; 如果它是最后一帧，则关闭当前帧并终止 emacs 的函数
(defun my/kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."

  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x N")        'my/make-frame)
(global-set-key (kbd "C-x C-c")    'my/kill-emacs)
(global-set-key (kbd "M-`")        'other-frame)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;;------------------------------
;; 更智能的填充/取消填充命令
(defun my/fill-unfill ()
  "Like `fill-paragraph', but unfill if used twice."

  (interactive)
  (let ((fill-column
         (if (eq last-command #'my/fill-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key (kbd "M-q")  'my/fill-unfill)

;;;------------------------------
;;;------------------------------
;;;------------------------------
;;;------------------------------
;;;------------------------------

(provide 'init-binds)
