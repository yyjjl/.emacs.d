(with-eval-after-load 'shm
  (defun shm/tab-or-close ()
    (interactive)
    (if (looking-at ")\\|]\\|}\\|`")
        (forward-char 1)
      (shm/tab)))
  (bind-keys :map shm-map
             ("C-c C-^") ("C-c 6" . shm/swing-up)
             ("C-c C-_") ("C-c -" . shm/insert-underscore)
             ("C-c C-e") ("C-c C-j")
             ("C-c e" . shm/export)
             ("C-c j" . shm/swing-down)
             ("<tab>" . shm/tab-or-close)))

(with-eval-after-load 'hindent
  (defun force-hindent-indent-size (org-fn)
    (list* "--tab-size" "4" (apply org-fn '())))
  (advice-add 'hindent-extra-arguments :around  #'force-hindent-indent-size))

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
   haskell-stylish-on-save t)
  (setq haskell-font-lock-symbols-alist
        (append haskell-font-lock-symbols-alist
                '(("*" . "×")
                  ("/" . "÷"))))

  (bind-keys :map haskell-mode-map
             ("C-c s" . ghc-sort-lines)
             ("C-'" . ghc-complete)
             ("C-c C-s" . ghc-case-split)
             ("M-." . haskell-mode-tag-find)
             ("C-c C-=") ("C-c =" . haskell-indent-insert-equal)
             ("C-c C-o") ("C-c o" . haskell-indent-insert-otherwise)
             ("C-c C-." . haskell-indent-put-region-in-literate)
             ("C-c ." . haskell-indent-align-guards-and-rhs)
             ("C-c C-|") ("C-c \\" . haskell-indent-insert-guard)
             ("C-c a c" . haskell-compile)
             ("C-c C-l" . haskell-process-load-file)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c k" . haskell-interactive-mode-clear)
             ("C-c a b" . haskell-process-cabal-build)
             ("C-c a a" . haskell-process-cabal)
             ("C-c M-n") ("C-c C-n" . ghc-goto-next-hole)
             ("C-c M-p") ("C-c C-p" . ghc-goto-prev-hole))
  (add-hook 'haskell-mode-hook
            (lambda ()
              (rainbow-delimiters-mode 1)
              (unless (is-buffer-file-temp)
                (ghc-init)
                (hare-init)
                (define-key haskell-mode-map
                  (kbd "C-c <tab>") 'haskell-process-do-info)
                (define-key haskell-mode-map
                  (kbd "C-c C-t") 'haskell-process-do-type)
                (add-to-list 'company-backends 'company-ghc)
                (add-to-list 'company-backends 'company-cabal)
                (add-to-list 'company-backends 'company-ghci)
                (haskell-doc-mode 1))
              (hindent-mode 1)
              ;; haskell-indentation-mode is incompatible with shm
              ;; (haskell-indentation-mode -1)
              (turn-on-haskell-indent)
              (structured-haskell-mode 1))))

(with-eval-after-load 'haskell-cabal
  (bind-keys :map haskell-cabal-mode-map
             ("C-c a c" . haskell-compile)
             ("C-c a a" . haskell-process-cabal )
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c k" . haskell-interactive-mode-clear)))

(provide 'init-haskell)