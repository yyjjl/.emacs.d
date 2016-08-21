(with-eval-after-load 'shm
  (setq shm-program-name "~/.cabal/bin/structured-haskell-mode"))

(with-eval-after-load 'haskell-mode
  (setq haskell-font-lock-symbols t)
  (add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)

            (flycheck-mode 1)
            (structured-haskell-mode 1)

            (haskell-doc-mode)
            (turn-on-haskell-indent)
            (define-key haskell-mode-map
              (kbd "C-'") 'company-complete))))



(provide 'init-haskell)
