;;; -*- lexical-binding: t; -*-

(setvar!
 bash-language-server-executable (executable-find "bash-language-server"))

(add-auto-mode! 'sh-mode
  "\\.basj_profile\\'" "\\.bash_history\\'"
  "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
  "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")

(define-hook! sh|setup (sh-mode-hook)
  (unless (buffer-temporary-p)
    (when bash-language-server-executable
      (add-transient-hook!
          (hack-local-variables-hook :local t :name sh|setup-internal)
        (lsp)
        (lsp//ui-doc-toggle 1)

        (setq-local lsp-eldoc-render-all nil)
        (setq-local lsp-eldoc-hook '(lsp-document-highlight lsp-hover))
        (setq-local company-lsp-cache-candidates 'auto)))

    (add-to-list 'flycheck-disabled-checkers 'sh-shellcheck)
    (flycheck-mode 1)))

(with-eval-after-load 'make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode))

(provide 'init-sh)
