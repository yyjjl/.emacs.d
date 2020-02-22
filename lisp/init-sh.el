;;; -*- lexical-binding: t; -*-

(define-variable! (bash-language-server :name bash-language-server-executable))

(add-auto-mode! 'sh-mode
  "\\.basj_profile\\'" "\\.bash_history\\'"
  "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
  "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")

(define-hook! sh|setup (sh-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (lsp//try-enable
     sh|setup-internal
     :enable bash-language-server-executable
     :init (setq-local lsp-eldoc-render-all nil))

    (add-to-list 'flycheck-disabled-checkers 'sh-shellcheck)
    (flycheck-mode 1)))

(with-eval-after-load 'make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode))

(provide 'init-sh)
