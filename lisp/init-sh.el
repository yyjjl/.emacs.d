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
      (lsp)
      (lsp//ui-doc-toggle 1)
      (setq-local lsp--disable-eldoc-in-minibuffer t)
      (setq-local lsp-eldoc-hook '(lsp-document-highlight lsp-hover))
      (setq-local company-lsp-cache-candidates 'auto))
    (flycheck-mode 1)
    ;; (add-to-list 'company-backends 'company-files)
    ))

(with-eval-after-load 'make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode))

(provide 'init-sh)
