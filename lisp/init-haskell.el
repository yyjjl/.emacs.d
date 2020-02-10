;;; -*- lexical-binding: t; -*-

(setvar!
 haskell-has-hie-p (executable-find "hie")
 haskell-has-stylish-haskell-p (executable-find "stylish-haskell")
 haskell-has-hindent-p (executable-find "hindent")
 haskell-has-cabal-p (executable-find "cabal"))

(require-packages!
 align
 haskell-mode
 (lsp-haskell :when haskell-has-hie-p)
 (hindent :when haskell-has-hindent-p)
 (company-cabal :when haskell-has-cabal-p))



(defun haskell*override-hindent ()
  "Work with `align'"
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (let ((start-end (hindent-decl-points)))
        (when start-end
          (setq beg (car start-end)
                end (cdr start-end))
          (hindent-reformat-region beg end t))))
    (align beg end)))

(define-hook! haskell|cabal-setup (haskell-cabal-mode-hook)
  (rainbow-delimiters-mode 1)
  (company//add-backend 'company-cabal))

(define-hook! haskell|setup (haskell-mode-hook)
  (rainbow-delimiters-mode 1)
  (haskell-decl-scan-mode 1)

  (when (buffer-enable-rich-feature-p)
    ((lsp//try-enable
      haskell|setup-internal
      :enable haskell-has-hie-p
      :fallback (progn
                  (haskell-doc-mode 1)
                  (flycheck-mode -1)))))

  (haskell-indentation-mode 1)
  (when haskell-has-hindent-p
    (hindent-mode 1)))


(with-eval-after-load 'hindent
  (advice-add 'hindent-reformat-decl :override #'haskell*override-hindent))

(with-eval-after-load 'haskell-mode
  (require 'haskell-indent)
  (require 'haskell-debug)

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

  (define-key! :map haskell-mode-map
    ([f5] . haskell-debug)
    ([f9] . haskell-compile)
    ("C-c F" . haskell-mode-stylish-buffer)
    ("C-c R" . haskell-mode-generate-tags)
    ("C-c j i" . haskell-navigate-imports)
    ("M-." . haskell-mode-tag-find)

    ("C-c C-." . haskell-indent-put-region-in-literate)
    ("C-c C-\\" . haskell-indent-insert-guard)
    ("C-c C-z" . haskell-interactive-switch)

    ("C-c L" . haskell-process-load-file)
    ("C-c C-t" . haskell-process-do-type)
    ("C-c p b" . haskell-process-cabal-build)
    ("C-c p a" . haskell-process-cabal)

    ("M-n" . next-error)
    ("M-p" . previous-error)))

(with-eval-after-load 'haskell-debug
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

(with-eval-after-load 'haskell-cabal
  (define-key! :map haskell-cabal-mode-map
    ("C-c a c" . haskell-compile)
    ("C-c a a" . haskell-process-cabal)
    ("C-c C-c" . haskell-process-cabal-build)
    ("C-c C-z" . haskell-interactive-switch)
    ("C-c C-k" . haskell-interactive-mode-clear)))

(with-eval-after-load 'haskell-font-lock
  ;; Do not use too much symbols
  (setq haskell-font-lock-symbols-alist nil))

(with-eval-after-load 'align
  (setq align-region-separate 'group)
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\(?:\\s-+\\|$\\)")
                 (modes quote (haskell-mode
                               literate-haskell-mode
                               idris-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-comments
                 (regexp . "\\(\\s-+\\)\\(--\\)\\s-+")
                 (modes quote (haskell-mode
                               literate-haskell-mode
                               idris-mode)))))


(provide 'init-haskell)
