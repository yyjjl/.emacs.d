;; -*- lexical-binding:t -*-

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'core-lib)

(load-feature! default)

(ymacs//try-load-file custom-file (expand-etc! "custom-template.el"))

(load-feature! package)
(load-feature! ui)

(executable! fcitx)

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
 (fcitx :when ymacs-fcitx-path)
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

(define-hook! ymacs|idle-init-emacs (ymacs-after-init-idle-hook)
  (when (and ymacs-fcitx-path (display-graphic-p))
    (fcitx-aggressive-setup))

  (find-file-noselect (expand-var! "org/*note*"))
  (find-file-noselect (expand-var! "org/*task*"))

  (desktop-save-mode 1))

(define-hook! ymacs|init-emacs (after-init-hook)
  (shackle-mode 1)
  (recentf-mode 1)
  (winner-mode 1)

  (session-initialize)

  (ivy-mode 1)
  (counsel-mode 1)
  (projectile-mode 1)
  (counsel-projectile-mode 1)

  (yas-global-mode 1)
  (global-company-mode 1)
  (semantic-mode 1)

  (which-key-mode 1)

  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)

  (global-whitespace-mode 1)
  (global-hl-todo-mode 1)
  (electric-indent-mode 1)

  (ymacs-pacakge//init))
