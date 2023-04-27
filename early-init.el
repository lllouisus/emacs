;;; early-init.el --- Pre init config
;;; Code:

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)



;; about save
;; Create the autosave and backup directories.
(defvar my-auto-save-folder "~/.cache/emacs/auto-save/")  ;;folder for auto-saves
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save/.saves-")  ;;set prefix for auto-saves 
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))) ;;location for all auto-save files

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo")))

;; https://stackoverflow.com/questions/15302973/emacs-auto-save-why-are-files-not-stored-in-the-correct-folder
(add-to-list 'auto-save-file-name-transforms
             (list "\\(.+/\\)*\\(.*?\\)" (expand-file-name "\\2" my-auto-save-folder)) t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.cache/emacs/bk"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      package-user-dir (expand-file-name "packages" user-emacs-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Native compilation settings
(when (fboundp 'native-comp-available-p)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq native-comp-eln-load-path (list path)
          native-compile-target-directory path))
  (setq native-comp-async-report-warnings-errors nil ;; Silence compiler warnings as they can be pretty disruptive
        inhibit-automatic-native-compilation     t)  ;; Make native compilation happens asynchronously
  )

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" (file-name-directory load-file-name)))


;; Prefer loading newest compiled .el file
(setq load-prefer-newer         noninteractive
      package-enable-at-startup t
      inhibit-startup-message   t)

(set-frame-font   "Hack Nerd Font" "Font settings")
(set-fontset-font "fontset-default" 'unicode "Hack Nerd Font")
(set-fontset-font t nil (font-spec :size 16 :name "Noto Color Emoji"))
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-default-coding-systems 'utf-8) ;; Set default coding system (especially for Windows)

(setq default-frame-alist
        '(
          (alpha 100 100)
          (cursor-color             . "#BE81F7")
          (font                     . "Hack Nerd Font")
          (tool-bar-lines           . 0)
          (inhibit-double-buffering . t)
          (vertical-scroll-bars     . right)))

(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(blink-cursor-mode              1)
(column-number-mode             t)
(global-font-lock-mode          1)
(menu-bar-mode                  -1)
(scroll-bar-mode               -1)
(tool-bar-mode                  -1)
;;(toggle-scroll-bar -1)
(tooltip-mode                   -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)


;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;;; Commentary:
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;;; early-init.el ends here
