;;; -*- lexical-binding: t; -*-

(add-auto-mode! 'sh-mode
  "\\.basj_profile\\'" "\\.bash_history\\'"
  "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
  "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")

(define-hook! sh|setup (sh-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (lsp//try-enable sh|setup-internal
      :init (setq-local lsp-eldoc-render-all nil))

    (add-to-list 'flycheck-disabled-checkers 'sh-shellcheck)
    (flycheck-mode 1)))

(config! make-mode
  :hook (whitespace-mode (makefile-mode-hook)))

(provide 'init-sh)
