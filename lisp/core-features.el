;; -*- lexical-binding: t; -*-

;; Core packages
(require-packages!
 cnfonts
 helpful
 yasnippet
 yasnippet-snippets
 flycheck
 ;; Code completion framework
 company
 ;; company-statistics
 ;; Save session to disk
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
 (fcitx :when emacs-use-fcitx-p)
 ;; Numbering windows
 winum
 ;; Highlight braces with their depth
 rainbow-delimiters
 ;; Highlight indentation
 highlight-indentation
 ;; ^L beautifier
 page-break-lines
 evil-nerd-commenter
 shackle
 symbol-overlay
 hl-todo
 expand-region)

(require 'core-popups)
(require 'core-ivy)
(require 'core-company)
(require 'core-term)
(require 'core-misc)
(require 'core-hydra)
(require 'core-semantic)
(require 'core-hideshow)

(cnfonts-enable)

(define-hook! package|idle-init-emacs (after-init-idle-hook)
  (when (and emacs-use-fcitx-p (display-graphic-p))
    (fcitx-aggressive-setup))

  (find-file-noselect (expand-var! "org/*note*"))
  (find-file-noselect (expand-var! "org/*task*"))

  (desktop-save-mode 1))

(define-hook! package|init-emacs (after-init-hook)
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

  (setq package-selected-packages
        (hash-table-keys package--required-packages))

  (add-to-list 'recentf-exclude (file-truename package-user-dir)))

(provide 'core-features)
