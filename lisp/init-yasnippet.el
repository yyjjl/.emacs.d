(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (add-to-list 'yas-snippet-dirs emacs-yasnippet-extra-dir)
  (setq-default mode-require-final-newline nil)
  (setq yas-prompt-functions '(yas-completing-prompt)))

(bind-keys ("M-s c" . aya-create)
           ("M-s y" . aya-expand))
(provide 'init-yasnippet)
