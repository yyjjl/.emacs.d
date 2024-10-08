;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-key! :map haskell-mode-map
    ([f9] . haskell-compile)
    ("C-c V" . haskell-cabal-visit-file)
    ("C-c D" . haskell-cabal-add-dependency)

    ("C-c C-b" . haskell-mode-stylish-buffer)

    ("C-c Q" . haskell-session-kill)
    ("C-c R" . haskell-mode-generate-tags)
    ("C-c j" . haskell-navigate-imports)
    ("C-." . haskell-mode-tag-find)

    ("C-c C-." . haskell-indent-put-region-in-literate)
    ("C-c C-\\" . haskell-indent-insert-guard)
    (("C-c C-z" "C-c z") . haskell-interactive-switch)

    ("C-c C-l" . haskell-process-load-file)
    ("C-c C-t" . haskell-process-do-type)
    ("C-c p b" . haskell-process-cabal-build)
    ("C-c p a" . haskell-process-cabal))

  (setq haskell-font-lock-symbols t)
  ;; Remove annoying error popups
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-tags-on-save nil)
  (setq haskell-stylish-on-save nil))

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
