;; Haskell
(defvar! haskell-hare-path
  (ignore-errors (find-library-in-directory "hare.el" "~/.cabal/share"))
  "Hare is a haskell refacting tool")
(defvar! haskell-has-stylish-haskell-p
  (executable-find "stylish-haskell")
  "Like clang-format, format haskell file")
(defvar! haskell-has-hastags-p (executable-find "hastags")
  "Tag haskell file, jump to defination")
(defvar! haskell-has-ghc-mod-p (executable-find "ghc-mod")
  "Haskell context sensitive completions")
(defvar! haskell-has-shm-p (executable-find "structured-haskell-mode")
  "Great tool to edit haskell file")
(defvar! haskell-has-hindent-p (executable-find "hindent")
  "Indent haskell expression")
(defvar! haskell-has-cabal-p (executable-find "cabal")
  "Haskell cabal support")
(defvar! haskell-has-idris-p (executable-find "idris")
  "Idris language support")

(require! 'haskell-mode)
(when haskell-has-ghc-mod-p
  (require! 'company-ghc)
  (require! 'ghc))
(when haskell-has-shm-p
  (require! 'shm))
(when haskell-has-hindent-p
  (require! 'hindent))
(when haskell-has-cabal-p
  (require! 'company-cabal))
(when haskell-has-idris-p
  (require! 'idris-mode))



(with-eval-after-load 'shm
  (defun haskell*shm-tab-or-close (fn &rest args)
    (if (looking-at ")\\|]\\|}\\|`")
        (forward-char 1)
      (apply fn args)))
  (advice-add 'shm/tab :around #'haskell*shm-tab-or-close)

  (define-key! :map shm-map
    ("C-." . shm/forward-node)
    ("C-," . shm/backward-node)
    ("C-?" . shm/describe-mode)
    ("C-c C-^") ("C-c 6" . shm/swing-up)
    ("C-c C-_") ("C-c -" . shm/insert-underscore)
    ("C-c C-e") ("C-c C-j")
    ("C-c e" . shm/export)
    ("C-c j" . shm/swing-down)))

(with-eval-after-load 'hindent
  ;; Rewrite function
  (defun hindent-reformat-decl ()
    "Work with `align'"
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
        (let ((beg (car start-end))
              (end (cdr start-end)))
          (hindent-reformat-region beg end t)
          (align beg end)))))

  (defun haskell*force-indent-size ($fn)
    (list* "--tab-size" "4" (apply $fn '())))
  (advice-add 'hindent-extra-arguments :around #'haskell*force-indent-size))

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
   haskell-stylish-on-save nil
   company-ghc-show-info t)

  (when haskell-hare-path
    (autoload 'hare-init haskell-hare-path nil t)
    (hare-init))
  (remap! "C-c C-r" "C-c r" haskell-mode-map)

  (define-key! :map haskell-mode-map
    ("C-c C-d" . ghc-browse-document)
    ("C-c b" . haskell-mode-stylish-buffer)
    ("C-c s" . haskell-sort-imports)
    ("M-." . haskell-mode-tag-find)
    ("C-c C-." . haskell-indent-put-region-in-literate)
    ("C-c ." . haskell-indent-align-guards-and-rhs)
    ("C-c C-|") ("C-c \\" . haskell-indent-insert-guard)
    ("C-c a c" . haskell-compile)
    ("C-c C-l" . haskell-process-load-file)
    ("C-c C-z" . haskell-interactive-switch)
    ("C-c k" . haskell-interactive-mode-clear)
    ("C-c a b" . haskell-process-cabal-build)
    ("C-c a a" . haskell-process-cabal)
    ("C-c '" . company-ghc-complete-by-hoogle)
    ("C-c ;" . company-ghc-complete-in-module)
    ("C-c C-c" . company-ghc-clear-failed-cache))

  (define-hook! haskell|cabal-setup (haskell-cabal-mode-hook)
    (rainbow-delimiters-mode 1)
    (add-to-list 'company-backends 'company-cabal))

  (define-hook! haskell|setup (haskell-mode-hook)
    (rainbow-delimiters-mode 1)
    (haskell-decl-scan-mode 1)
    (unless (buffer-temporary?)
      (when haskell-has-ghc-mod-p
        (ghc-init)
        (add-to-list 'company-backends 'company-ghc))

      (define-key haskell-mode-map
        (kbd "C-c <tab>") 'haskell-process-do-info)
      (define-key haskell-mode-map
        (kbd "C-c C-t") 'haskell-process-do-type)

      (haskell-doc-mode 1))

    (when haskell-has-hindent-p
      (hindent-mode 1))

    (turn-on-haskell-indent)
    (if haskell-has-shm-p
        (structured-haskell-mode 1)
      ;; haskell-indentation-mode is incompatible with shm
      (haskell-indentation-mode 1))
    (hl-line-mode -1)))

(with-eval-after-load 'ghc-check
  (setq ghc-check-command 'hlint)
  (fset 'ghc-check-syntax (lambda () "Do nothing")))

(with-eval-after-load 'haskell-cabal
  (define-key! :map haskell-cabal-mode-map
             ("C-c a c" . haskell-compile)
             ("C-c a a" . haskell-process-cabal )
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-k" . haskell-interactive-mode-clear)))

(with-eval-after-load 'haskell-font-lock
  ;; Do not use too much symbols
  ;; Keep this single-character symbols
  (setq haskell-font-lock-symbols-alist
        '(("\\" . "λ")
          ("*" . "×")
          ("/" . "÷")
          ("." "∘" haskell-font-lock-dot-is-not-composition))))

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
