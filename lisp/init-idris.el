;;; -*- lexical-binding: t; -*-

(define-variable! :pkg idris idris)

(require-packages!
 haskell-mode
 (idris-mode :when idris-use-idris-p))

(defun idris//setup-view-keys (-map)
  (define-key! :map -map
    ("<tab>" . forward-button)
    ("<backtab>" . backward-button)
    ("SPC" . scroll-up)
    ("e" . scroll-up-line)
    ("y" . scroll-down-line)
    ("u" . scroll-down)
    ("n" . next-line)
    ("p" . previous-line)))

(defun idris/mark-paragraph ()
  (interactive)
  (save-mark-and-excursion
    (mark-paragraph)
    (call-interactively #'align)))

(config! idris-mode
  :bind
  (:map idris-mode-map
   ("C-c L" . idris-list-holes)
   ("C-c ." . idris-print-definition-of-name)
   ("C-c C-." . idris-print-definition-of-name)
   ("C-c C-/" . idris-browse-namespace)
   ("M-q" . idris/mark-paragraph))

  :hook
  (setup
   :define (idris-mode-hook)
   (setq-local company-idle-delay nil))

  (core|generic-comint-mode-setup (idris-repl-mode-hook))

  :config
  (add-to-list 'core-popups-help-modes 'idris-info-mode :append)
  (add-to-list 'core-popups-help-modes 'idris-compiler-notes-mode :append))

(config! idris-hole-list
  :config
  (idris//setup-view-keys idris-hole-list-mode-map))

(config! idris-tree-info
  :config
  (idris//setup-view-keys idris-tree-info-mode-map))

(config! idris-info
  :hook
  (setup
   :define (idris-info-mode-hook)
   (setq-local eldoc-documentation-function 'idris-eldoc-lookup))

  :config
  (idris//setup-view-keys idris-info-mode-map))

(provide 'init-idris)
