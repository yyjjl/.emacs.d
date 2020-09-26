;; -*- lexical-binding:t -*-

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'core-lib)

(load-feature! default)

(ymacs//try-load-file custom-file (expand-etc! "custom-template.el"))

(load-feature! package)
(load-feature! ui)

;; Core packages
(require-packages!
 yasnippet
 yasnippet-snippets
 flycheck
 company
 session
 hydra
 pretty-hydra
 ivy
 ivy-hydra
 counsel
 swiper
 projectile
 counsel-projectile
 ;; `counsel-M-x' need amx to get history
 amx
 ;; Show key bindings when pressing
 which-key
 ;; Highlight braces with their depth
 rainbow-delimiters
 ;; Highlight indentation
 highlight-indentation
 ;; ^L beautifier
 page-break-lines
 evil-nerd-commenter
 shackle
 hl-todo
 expand-region)

(load-feature! popup)
(load-feature! completion_ivy)
(load-feature! company)
(load-feature! term)
(load-feature! misc)
(load-feature! hydra)
(load-feature! semantic)
(load-feature! hideshow)
(load-feature! edit)
(load-feature! dired)
(load-feature! ibuffer)

(ymacs//try-load-file features-file (expand-etc! "features-template.el"))
