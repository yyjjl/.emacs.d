;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode))

(after-feature! lisp
  (advice-add 'ymacs-lisp|common-setup :after
              (lambda (&rest _)
                (rainbow-delimiters-mode 1))))
