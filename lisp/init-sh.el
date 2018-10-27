;;; -*- lexical-binding: t; -*-

(add-auto-mode! 'sh-mode
  "\\.basj_profile\\'" "\\.bash_history\\'"
  "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
  "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")

(add-hook 'sh-mode-hook
          (lambda ()
            (unless (buffer-temporary-p)
              (flycheck-mode 1)
              (add-to-list 'company-backends 'company-files))))

(add-hook 'makefile-mode #'whitespace-mode)

(provide 'init-sh)
