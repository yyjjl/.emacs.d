;; Haskell
(setvar!
 haskell-hare-path (ignore-errors
                     (find-library-in-directory "hare.el" "~/.cabal/share"))
 haskell-has-stylish-haskell-p (executable-find "stylish-haskell")
 haskell-has-stack-p (executable-find "stack")
 haskell-has-hindent-p (executable-find "hindent")
 haskell-has-cabal-p (executable-find "cabal")
 haskell-has-idris-p (executable-find "idris"))

(require-packages!
 haskell-mode
 (intero :when haskell-has-stack-p)
 (hindent :when haskell-has-hindent-p)
 (company-cabal :when haskell-has-cabal-p)
 (idris-mode :when haskell-has-idris-p))



(with-eval-after-load 'hindent
  ;; Rewrite function
  (defun hindent-reformat-decl ()
    "Work with `align'"
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning)
                end (region-end))
        (let ((start-end (hindent-decl-points)))
          (when start-end
            (setq beg (car start-end)
                  end (cdr start-end))
            (hindent-reformat-region beg end t))))
      (align beg end))))

(with-eval-after-load 'haskell-mode
  (require 'haskell-indent)

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

  (when haskell-hare-path
    (add-to-list 'load-path (file-name-directory haskell-hare-path))
    (autoload 'hare-init haskell-hare-path nil t)
    (hare-init))
  (remap! "C-c C-r" "C-c r" haskell-mode-map)

  (define-key! :map haskell-mode-map
    ([f5] . haskell-debug)
    ([f10] . haskell-compile)
    ("C-c b" . haskell-mode-stylish-buffer)
    ("C-c s" . haskell-sort-imports)
    ("M-." . haskell-mode-tag-find)
    ("C-c C-." . haskell-indent-put-region-in-literate)
    ("C-c ." . haskell-indent-align-guards-and-rhs)
    ("C-c C-o" . haskell-indent-insert-otherwise)
    ("C-c C-|")
    ("C-c <tab>" . haskell-process-do-info)
    ("C-c C-t" . haskell-process-do-type)
    ("C-c C-\\" . haskell-indent-insert-guard)
    ("C-c \\" . haskell-indent-insert-guard)
    ("C-c C-l" . haskell-process-load-file)
    ("C-c C-z" . haskell-interactive-switch)
    ("C-c k" . haskell-interactive-mode-clear)
    ("C-c a b" . haskell-process-cabal-build)
    ("C-c a a" . haskell-process-cabal)
    ("C-c R" . haskell-mode-generate-tags))

  (define-hook! haskell|cabal-setup (haskell-cabal-mode-hook)
    (rainbow-delimiters-mode 1)
    (add-to-list 'company-backends 'company-cabal))

  (define-hook! haskell|setup (haskell-mode-hook)
    (rainbow-delimiters-mode 1)
    (haskell-decl-scan-mode 1)

    (unless (buffer-temporary?)
      (define-key! :map haskell-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))

      (haskell-doc-mode 1)
      (if (and haskell-has-stack-p
               (locate-dominating-file default-directory "stack.yaml"))
          (progn
            (intero-mode 1)
            (setq-local haskell-doc-show-prelude nil))
        (setq-local haskell-doc-show-prelude t)
        (flycheck-mode -1)))

    (when haskell-has-hindent-p
      (hindent-mode 1))
    (haskell-indentation-mode 1)))

(with-eval-after-load 'haskell-cabal
  (define-key! :map haskell-cabal-mode-map
    ("C-c a c" . haskell-compile)
    ("C-c a a" . haskell-process-cabal )
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
