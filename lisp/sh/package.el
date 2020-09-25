;;; -*- lexical-binding: t; -*-

(require-packages! lsp-mode)

(add-auto-mode! 'sh-mode
  "\\.basj_profile\\'" "\\.bash_history\\'"
  "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
  "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")
