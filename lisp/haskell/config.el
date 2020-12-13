;;; -*- lexical-binding: t; -*-

(autoload #'haskell-indent-put-region-in-literate "haskell-indent" nil t)
(autoload #'haskell-indent-insert-guard "haskell-indent" nil t)

(after! lsp-haskell
  (ymacs-lsp//set-simple-install-fn
   'lsp-haskell
   "ghcup install hls"))

(after! haskell-mode
  (define-key! :map haskell-mode-map
    ([f5] . haskell-debug)
    ([f9] . haskell-compile)

    ("C-c C-b" . lsp-format-buffer)
    ("C-c b" . lsp-format-buffer)
    ("C-c Q" . haskell-session-kill)
    ("C-c R" . haskell-mode-generate-tags)
    ("C-c j" . haskell-navigate-imports)
    ("C-." . haskell-mode-tag-find)

    ("C-c C-." . haskell-indent-put-region-in-literate)
    ("C-c C-\\" . haskell-indent-insert-guard)
    ("C-c C-z" . haskell-interactive-switch)

    ("C-c C-l" . haskell-process-load-file)
    ("C-c C-t" . haskell-process-do-type)
    ("C-c p b" . haskell-process-cabal-build)
    ("C-c p a" . haskell-process-cabal))

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
   haskell-stylish-on-save nil))

(after! haskell-debug
  (define-key! :map haskell-mode-map :prefix "C-c d"
    ("b" . haskell-debug/break-on-function)
    ("n" . haskell-debug/next)
    ("s" . haskell-debug/step)
    ("t" . haskell-debug/trace)
    ("d" . haskell-debug/delete)
    ("S" . haskell-debug/select)
    ("a" . haskell-debug/abandon)
    ("g" . haskell-debug/refresh)
    ("c" . haskell-debug/continue)
    ("p" . haskell-debug/previous)
    ("r" . haskell-debug/start-step)
    ("N" . haskell-debug/breakpoint-numbers)))
