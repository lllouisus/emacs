;;; -*- lexical-binding: t; no-byte-compile: t; -*-


(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; move
;; (global-unset-key (kbd "C-p"))
;; (global-unset-key (kbd "C-n"))
;; (global-unset-key (kbd "C-f"))
;; (global-unset-key (kbd "C-b"))
;; (global-unset-key (kbd "C-u"))
;; (global-unset-key (kbd "C-e"))
;; (global-unset-key (kbd "C-i"))

(global-set-key (kbd "C-c s k b") 'ws-begin-block)
(global-set-key (kbd "C-c s k k") 'ws-end-block)
(global-set-key (kbd "C-c s k m") 'ws-move-block)
(global-set-key (kbd "C-c s k c") 'ws-copy-block)
;; (global-set-key (kbd "C-e") 'next-line)
;; (global-set-key (kbd "C-n") 'backward-char)
;; (global-set-key (kbd "C-i") 'forward-char)

;; wordstar special functions:



(defun ws-error (string)
  "Report error of a WordStar special function. Error message is saved
in ws-last-errormessage for recovery with C-q w."
  (setq ws-last-errormessage string)
  (error string))

(defun ws-set-marker-0 ()
  "In WordStar mode: Set marker 0 to current cursor position."
  (interactive)
  (setq ws-marker-0 (point-marker))
  (message "Marker 0 set"))

(defun ws-set-marker-1 ()
  "In WordStar mode: Set marker 1 to current cursor position."
  (interactive)
  (setq ws-marker-1 (point-marker))
  (message "Marker 1 set"))

(defun ws-set-marker-2 ()
  "In WordStar mode: Set marker 2 to current cursor position."
  (interactive)
  (setq ws-marker-2 (point-marker))
  (message "Marker 2 set"))

(defun ws-set-marker-3 ()
  "In WordStar mode: Set marker 3 to current cursor position."
  (interactive)
  (setq ws-marker-3 (point-marker))
  (message "Marker 3 set"))

(defun ws-set-marker-4 ()
  "In WordStar mode: Set marker 4 to current cursor position."
  (interactive)
  (setq ws-marker-4 (point-marker))
  (message "Marker 4 set"))

(defun ws-set-marker-5 ()
  "In WordStar mode: Set marker 5 to current cursor position."
  (interactive)
  (setq ws-marker-5 (point-marker))
  (message "Marker 5 set"))

(defun ws-set-marker-6 ()
  "In WordStar mode: Set marker 6 to current cursor position."
  (interactive)
  (setq ws-marker-6 (point-marker))
  (message "Marker 6 set"))

(defun ws-set-marker-7 ()
  "In WordStar mode: Set marker 7 to current cursor position."
  (interactive)
  (setq ws-marker-7 (point-marker))
  (message "Marker 7 set"))

(defun ws-set-marker-8 ()
  "In WordStar mode: Set marker 8 to current cursor position."
  (interactive)
  (setq ws-marker-8 (point-marker))
  (message "Marker 8 set"))

(defun ws-set-marker-9 ()
  "In WordStar mode: Set marker 9 to current cursor position."
  (interactive)
  (setq ws-marker-9 (point-marker))
  (message "Marker 9 set"))

(defun ws-begin-block ()
  "In WordStar mode: Set block begin marker to current cursor position."
  (interactive)
  (setq ws-block-begin-marker (point-marker))
  (message "Block begin marker set"))

(defun ws-show-markers ()
  "In WordStar mode: Show block markers."
  (interactive)
  (if (or ws-block-begin-marker ws-block-end-marker)
      (save-excursion
        (if ws-block-begin-marker
            (progn
              (goto-char ws-block-begin-marker)
              (message "Block begin marker")
              (sit-for 2))
          (message "Block begin marker not set")
          (sit-for 2))
        (if ws-block-end-marker
            (progn
              (goto-char ws-block-end-marker)
              (message "Block end marker")
              (sit-for 2))
          (message "Block end marker not set"))
        (message ""))
    (message "Block markers not set")))


(defun ws-indent-block ()
  "In WordStar mode: Indent block (not yet implemented)."
  (interactive)
  (ws-error "Indent block not yet implemented"))

(defun ws-end-block ()
  "In WordStar mode: Set block end marker to current cursor position."
  (interactive)
  (setq ws-block-end-marker (point-marker))
  (message "Block end marker set"))

(defun ws-print-block ()
  "In WordStar mode: Print block."
  (interactive)
  (message "Don't do this. Write block to a file (C-k w) and print this file."))

(defun ws-mark-word ()
  "In WordStar mode: Mark current word as block."
  (interactive)
  (save-excursion
    (forward-word 1)
    (sit-for 1)
    (ws-end-block)
    (forward-word -1)
    (sit-for 1)
    (ws-begin-block)))

(defun ws-exdent-block ()
  "I don't know what this (C-k u) should do."
  (interactive)
  (ws-error "This won't be done -- not yet implemented."))

(defun ws-move-block ()
  "In WordStar mode: Move block to current cursor position."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
        (kill-region ws-block-begin-marker ws-block-end-marker)
        (yank)
        (save-excursion
          (goto-char (region-beginning))
          (setq ws-block-begin-marker (point-marker))
          (goto-char (region-end))
          (setq ws-block-end-marker (point-marker))))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
                    (ws-block-end-marker "Block begin marker not set")
                    (t "Block markers not set")))))

(defun ws-write-block ()
  "In WordStar mode: Write block to file."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (let ((filename (read-file-name "Write block to file: ")))
        (write-region ws-block-begin-marker ws-block-end-marker filename))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
                    (ws-block-end-marker "Block begin marker not set")
                    (t "Block markers not set")))))


(defun ws-delete-block ()
  "In WordStar mode: Delete block."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
        (kill-region ws-block-begin-marker ws-block-end-marker)
        (setq ws-block-end-marker nil)
        (setq ws-block-begin-marker nil))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
                    (ws-block-end-marker "Block begin marker not set")
                    (t "Block markers not set")))))

(defun ws-find-marker-0 ()
  "In WordStar mode: Go to marker 0."
  (interactive)
  (if ws-marker-0
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-0))
    (ws-error "Marker 0 not set")))

(defun ws-find-marker-1 ()
  "In WordStar mode: Go to marker 1."
  (interactive)
  (if ws-marker-1
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-1))
    (ws-error "Marker 1 not set")))

(defun ws-find-marker-2 ()
  "In WordStar mode: Go to marker 2."
  (interactive)
  (if ws-marker-2
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-2))
    (ws-error "Marker 2 not set")))

(defun ws-find-marker-3 ()
  "In WordStar mode: Go to marker 3."
  (interactive)
  (if ws-marker-3
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-3))
    (ws-error "Marker 3 not set")))

(defun ws-find-marker-4 ()
  "In WordStar mode: Go to marker 4."
  (interactive)
  (if ws-marker-4
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-4))
    (ws-error "Marker 4 not set")))

(defun ws-find-marker-5 ()
  "In WordStar mode: Go to marker 5."
  (interactive)
  (if ws-marker-5
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-5))
    (ws-error "Marker 5 not set")))

(defun ws-find-marker-6 ()
  "In WordStar mode: Go to marker 6."
  (interactive)
  (if ws-marker-6
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-6))
    (ws-error "Marker 6 not set")))

(defun ws-find-marker-7 ()
  "In WordStar mode: Go to marker 7."
  (interactive)
  (if ws-marker-7
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-7))
    (ws-error "Marker 7 not set")))

(defun ws-find-marker-8 ()
  "In WordStar mode: Go to marker 8."
  (interactive)
  (if ws-marker-8
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-8))
    (ws-error "Marker 8 not set")))

(defun ws-find-marker-9 ()
  "In WordStar mode: Go to marker 9."
  (interactive)
  (if ws-marker-9
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-marker-9))
    (ws-error "Marker 9 not set")))

(defun ws-goto-block-begin ()
  "In WordStar mode: Go to block begin marker."
  (interactive)
  (if ws-block-begin-marker
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-block-begin-marker))
    (ws-error "Block begin marker not set")))

(defun ws-search (string)
  "In WordStar mode: Search string, remember string for repetition."
  (interactive "sSearch for: ")
  (message "Forward (f) or backward (b)")
  (let ((direction
         (read-char)))
    (cond ((equal (upcase direction) ?F)
           (setq ws-search-string string)
           (setq ws-search-direction t)
           (setq ws-last-cursorposition (point-marker))
           (search-forward string))
          ((equal (upcase direction) ?B)
           (setq ws-search-string string)
           (setq ws-search-direction nil)
           (setq ws-last-cursorposition (point-marker))
           (search-backward string))
          (t (keyboard-quit)))))

(defun ws-goto-block-end ()
  "In WordStar mode: Go to block end marker."
  (interactive)
  (if ws-block-end-marker
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-block-end-marker))
    (ws-error "Block end marker not set")))

(defun ws-undo ()
  "In WordStar mode: Undo and give message about undoing more changes."
  (interactive)
  (undo)
  (message "Repeat C-q l to undo more changes."))

(defun ws-goto-last-cursorposition ()
  "In WordStar mode: "
  (interactive)
  (if ws-last-cursorposition
      (progn
        (setq ws-last-cursorposition (point-marker))
        (goto-char ws-last-cursorposition))
    (ws-error "No last cursor position available.")))

(defun ws-last-error ()
  "In WordStar mode: repeat last error message.
This will only work for errors raised by WordStar mode functions."
  (interactive)
  (if ws-last-errormessage
      (message "%s" ws-last-errormessage)
    (message "No WordStar error yet.")))

(defun ws-kill-eol ()
  "In WordStar mode: Kill to end of line (like WordStar, not like Emacs)."
  (interactive)
  (let ((p (point)))
    (end-of-line)
    (kill-region p (point))))

(defun ws-kill-bol ()
  "In WordStar mode: Kill to beginning of line
\(like WordStar, not like Emacs)."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (kill-region (point) p)))

(defun kill-complete-line ()
  "Kill the complete line."
  (interactive)
  (beginning-of-line)
  (if (eobp) (error "End of buffer"))
  (let ((beg (point)))
    (forward-line 1)
    (kill-region beg (point))))

(defun ws-repeat-search ()
  "In WordStar mode: Repeat last search."
  (interactive)
  (setq ws-last-cursorposition (point-marker))
  (if ws-search-string
      (if ws-search-direction
          (search-forward ws-search-string)
        (search-backward ws-search-string))
    (ws-error "No search to repeat")))

(defun ws-query-replace (from to)
  "In WordStar mode: Search string, remember string for repetition."
  (interactive "sReplace:
sWith: " )
  (setq ws-search-string from)
  (setq ws-search-direction t)
  (setq ws-last-cursorposition (point-marker))
  (query-replace from to))

(defun ws-copy-block ()
  "In WordStar mode: Copy block to current cursor position."
  (interactive)
  (if (and ws-block-begin-marker ws-block-end-marker)
      (progn
        (copy-region-as-kill ws-block-begin-marker ws-block-end-marker)
        (yank)
        (save-excursion
          (goto-char (region-beginning))
          (setq ws-block-begin-marker (point-marker))
          (goto-char (region-end))
          (setq ws-block-end-marker (point-marker))))
    (ws-error (cond (ws-block-begin-marker "Block end marker not set")
                    (ws-block-end-marker "Block begin marker not set")
                    (t "Block markers not set")))))

(provide 'init-keymaps)
