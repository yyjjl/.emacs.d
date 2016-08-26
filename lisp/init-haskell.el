(with-eval-after-load 'shm
  (setq shm-program-name "~/.cabal/bin/structured-haskell-mode")
  (define-key shm-map (kbd "C-c C-s") 'shm/case-split))

(with-eval-after-load 'haskell-mode
  (setq haskell-font-lock-symbols t)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (ghc-init)

              (flycheck-mode 1)
              (haskell-doc-mode)
              ;; haskell-indentation-mode is incompatible with shm
              (haskell-indentation-mode -1)
              (turn-on-haskell-indent)
              (structured-haskell-mode 1)
              (define-key haskell-mode-map
                (kbd "C-'") 'company-complete))))



(provide 'init-haskell)
