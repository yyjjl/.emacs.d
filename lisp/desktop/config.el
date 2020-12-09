;; -*- lexical-binding:t -*-

(after! desktop
  ;; These minor modes are enabled by core
  (dolist (mode '(git-gutter-mode
                  ivy-mode
                  counsel-mode
                  projectile-mode
                  yas-minor-mode
                  company-mode
                  which-key-mode
                  subword-mode
                  global-auto-revert-mode
                  flycheck-mode
                  hs-minor-mode
                  auto-revert-mode
                  lsp-mode))
    (add-to-list 'desktop-minor-mode-table (cons mode nil))))
