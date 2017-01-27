(with-eval-after-load 'shm
  (setq shm-program-name "~/.cabal/bin/structured-haskell-mode")
  (define-key shm-map (kbd "C-c C-s") 'shm/case-split))

(with-eval-after-load 'haskell-mode
  (setq
   ;; Use notify.el (if you have it installed) at the end of running
   ;; Cabal commands or generally things worth notifying.
   haskell-font-lock-symbols t
   haskell-notify-p t
   ;; To enable tags generation on save.
   haskell-tags-on-save t
   ;; Remove annoying error popups
   haskell-interactive-popup-errors nil
   ;; Better import handling
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   ;; Disable haskell-stylish-on-save, as it breaks flycheck highlighting.
   ;; NOTE: May not be true anymore - taksuyu 2015-10-06
   haskell-stylish-on-save nil)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (ghc-init)
              (add-to-list 'company-backends '(company-ghc
                                               :with company-dabbrev-code))
              (add-to-list 'company-backends 'company-cabal)
              (haskell-doc-mode 1)
              (hindent-mode)
              ;; haskell-indentation-mode is incompatible with shm
              ;; (haskell-indentation-mode -1)
              (turn-on-haskell-indent)
              (structured-haskell-mode 1)
              (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
              (define-key haskell-mode-map
                (kbd "M-S") 'ghc-sort-lines)
              (define-key haskell-mode-map
                (kbd "C-'") 'ghc-complete))))

(with-eval-after-load 'hindent
  (setq hindent-process-path "~/.cabal/bin/hindent"))

(provide 'init-haskell)
