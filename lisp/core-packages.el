;; Core packages
(require-packages!
 yasnippet
 yasnippet-snippets
 (flycheck :archive "melpa-stable")
 ;; Code completion framework
 company
 company-statistics
 ;; Save session to disk
 session
 hydra
 ivy
 counsel
 ivy-hydra
 swiper
 projectile
 counsel-projectile
 ;; `counsel-M-x' need smex to get history
 smex
 ;; Show key bindings when pressing
 which-key
 (fcitx :when env-has-fcitx-p)
 ;; Numbering windows
 window-numbering
 ;; Highlight braces with their depth
 rainbow-delimiters
 ;; Highlight indentation
 highlight-indentation
 ;; ^L beautifier
 page-break-lines
 evil-nerd-commenter
 shackle
 hippie-exp-ext
 expand-region)

(require 'core-popups)
(require 'core-ivy)
(require 'core-company)
(require 'core-term)
(require 'core-misc)
(require 'core-hydra)

(defvar after-init-idle-hook '(package|idle-init-emacs))

(defun package|idle-init-emacs ()
  (when (and (display-graphic-p)
             env-has-fcitx-p)
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

  (which-key-mode 1)

  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)

  (setq package-selected-packages
        (hash-table-keys package--required-packages))
  (add-to-list 'recentf-exclude (file-truename package-user-dir))

  (run-hooks 'after-init-idle-hook)

  (run-with-timer 1 nil 'run-hooks 'after-init-idle-hook))

(provide 'core-packages)
