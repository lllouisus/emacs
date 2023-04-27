;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; Setup and keybindings for modal interaction with Emacs. Currently I'm using Meow instead of evil/vim bindings.

;;; Code:


(require 'autothemer)

(autothemer-deftheme
 mine "A theme to set the mood for Mineween"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (mine-black      "#000000")
  (mine-white      "#ffffff")
  (mine-orange     "orange1")
  (mine-dk-orange  "#eb6123")
  (mine-purple     "MediumPurple2")
  (mine-dk-purple  "MediumPurple4")
  (mine-green      "#719e07")
  (mine-nil         nil)
  (mine-mode-line  "#002b36")
  (mine-font       "#839496")
  (mine-fun        "#268bd2")
  (mine-cursor-line-bg "#000000")
  (mine-cursor-line-fg "#b58900")
  (mine-line-num   "#586e75")
  (mine-search-fg  "#c0c0c0")
  (mine-search-bg  "#b58900")
  (mine-include    "#d78700")
  (mine-keyword    "#b58900")
  
  (mine-lsp-error-fg    "#dc322f")
  (mine-lsp-error-bg    "#330d05")
  (mine-lsp-hint-fg    "#2aa198")
  (mine-lsp-hint-bg    "#0a3530")
  (mine-lsp-info-fg    "#268bd2")
  (mine-lsp-info-bg    "#003366")
  (mine-lsp-warn-fg    "#b58900")
  (mine-lsp-warn-bg    "#221704")
  (mine-grey           "#3d3d38")
  (mine-bg             "#031111")
  (mine-fg             "#839496")
  
  

  
  )

 ;; Customize faces
 ((default                   (:foreground mine-fg :background mine-bg))
  (cursor                    (:background mine-dk-orange))
  (region                    (:background mine-dk-purple))
  (mode-line                 (:background mine-mode-line :foreground mine-font))
  (font-lock-keyword-face    (:foreground mine-keyword))
  (font-lock-constant-face   (:foreground mine-fun))
  (font-lock-string-face     (:foreground mine-orange))
  (font-lock-builtin-face    (:foreground mine-fun))
  ;;(font-lock-regexp-grouping-backslash  (:foreground mine-font))
  (header-line               (:foreground mine-font))
  (line-number               (:foreground mine-line-num))
  (line-number-current-line  (:foreground mine-cursor-line-fg :background mine-cursor-line-bg))
  (ivy-current-match         (:background mine-mode-line))
  (font-lock-comment-delimiter-face  (:foreground mine-font))
  (font-lock-comment-face            (:foreground mine-font))
  (company-tooltip                   (:foreground mine-font :background mine-mode-line))  ;;cmpbar
  (company-tooltip-common            (:foreground mine-fun))
  (company-tooltip-common-selection  (:foreground mine-fun))
  (company-tooltip-quick-access      (:foreground mine-font))
  (company-tooltip-quick-access-selection      (:foreground mine-font))
  
  (ivy-minibuffer-match-face-1                 (:foreground mine-grey :background mine-nil))
  (ivy-minibuffer-match-face-2                 (:foreground mine-search-bg :background mine-nil))
  (ivy-minibuffer-match-face-3                 (:foreground mine-green :background mine-nil))
  (ivy-minibuffer-match-face-4                 (:foreground mine-purple :background mine-nil))
  (ivy-highlight-face                          (:background mine-nil))
  
  (isearch                           (:foreground mine-search-bg :background mine-nil))
  (isearch-group-1                   (:foreground mine-search-bg :background mine-nil))
  (swiper-line-face    (:background mine-nil))
  (swiper-match-face-1    (:foreground mine-nil :background mine-nil))
  (swiper-match-face-2    (:foreground mine-black :background mine-search-bg))
  (swiper-match-face-3    (:foreground mine-black :background mine-green))
  (swiper-match-face-4    (:foreground mine-black :background mine-purple))

  (marginalia-documentation  (:foreground mine-font))
  ;; (marginalia-file-name  (:foreground mine-font))
  ;; (marginalia-file-priv-no  (:foreground mine-font))



  ;; about lsp
  (font-lock-preprocessor-face                  (:foreground mine-include))
  (lsp-face-highlight-textual                   (:foreground mine-fun))
  (font-lock-type-face                          (:foreground mine-green))

  (error                 (:foreground mine-lsp-error-fg :background mine-lsp-error-bg))
  (lsp-treemacs-file-hint                  (:foreground mine-lsp-hint-fg :background mine-lsp-hint-bg))
  (lsp-treemacs-file-info                 (:foreground mine-lsp-info-fg :background mine-lsp-info-bg))
  (flycheck-verify-select-checker                 (:foreground mine-lsp-warn-fg :background mine-lsp-warn-bg))


  
  

  
  (org-level-1               (:foreground mine-orange))))

(provide-theme 'mine)
