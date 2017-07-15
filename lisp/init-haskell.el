(with-eval-after-load 'shm
  (defun shm/tab-or-close (fn &rest args)
    (if (looking-at ")\\|]\\|}\\|`")
        (forward-char 1)
      (apply fn args)))
  (advice-add 'shm/tab :around #'shm/tab-or-close)

  (bind-keys :map shm-map
             ("C-c C-^") ("C-c 6" . shm/swing-up)
             ("C-c C-_") ("C-c -" . shm/insert-underscore)
             ("C-c C-e") ("C-c C-j")
             ("C-c e" . shm/export)
             ("C-c j" . shm/swing-down)))

(with-eval-after-load 'hindent
  (defun hindent-reformat-decl ()
    "Work with `align'"
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
        (let ((beg (car start-end))
              (end (cdr start-end)))
          (hindent-reformat-region beg end t)
          (align beg end)))))

  (defun force-hindent-indent-size (org-fn)
    (list* "--tab-size" "4" (apply org-fn '())))
  (advice-add 'hindent-extra-arguments :around #'force-hindent-indent-size))

(with-eval-after-load 'haskell-mode
  (setq
   ;; Use notify.el (if you have it installed) at the end of running
   ;; Cabal commands or generally things worth notifying.
   haskell-font-lock-symbols t
   haskell-notify-p t
   ;; To enable tags generation on save.
   haskell-tags-on-save nil
   ;; Remove annoying error popups
   haskell-interactive-popup-errors nil
   ;; Better import handling
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-stylish-on-save nil)

  (hare-init)
  (remap-kbd "C-c C-r" "C-c r" haskell-mode-map)

  (bind-keys :map haskell-mode-map
             ("C-c b" . haskell-mode-stylish-buffer)
             ("C-c s" . haskell-sort-imports)
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
             ("C-c a a" . haskell-process-cabal))

  (defun haskell-mode-setup ()
    (rainbow-delimiters-mode 1)
    (haskell-decl-scan-mode 1)
    (unless (is-buffer-file-temp)
      (ghc-init)

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
    (structured-haskell-mode 1))

  (add-hook 'haskell-mode-hook #'haskell-mode-setup))

(with-eval-after-load 'haskell-cabal
  (bind-keys :map haskell-cabal-mode-map
             ("C-c a c" . haskell-compile)
             ("C-c a a" . haskell-process-cabal )
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c k" . haskell-interactive-mode-clear)))

(with-eval-after-load 'haskell-font-lock
  (setq haskell-font-lock-symbols-alist
        (append haskell-font-lock-symbols-alist
                '(("*" . "×")
                  ("/" . "÷")))))

(with-eval-after-load 'idris-mode
  (require 'haskell-font-lock)
  (font-lock-add-keywords 'idris-mode
                          (haskell-font-lock-symbols-keywords))
  (push 'idris-compiler-notes-mode
        popwin:special-display-config)
  (push '(idris-repl-mode
          :height 0.4
          :noselect nil
          :position bottom)
        popwin:special-display-config))

(with-eval-after-load 'align
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))


(provide 'init-haskell)