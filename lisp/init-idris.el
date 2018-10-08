;;; -*- lexical-binding: t; -*-

(setvar! idris-has-idris-p (executable-find "idris"))

(require-packages!
 haskell-mode
 (idris-mode :when idris-has-idris-p))



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

(with-eval-after-load 'idris-mode
  (add-hook 'idris-repl-mode-hook #'core|generic-comint-mode-setup)
  (add-hook 'idris-mode-hook
            (lambda () (setq-local company-idle-delay nil)))

  (define-key! :map idris-mode-map
    ("C-c L" . idris-list-holes)
    ("C-c ." . idris-print-definition-of-name)
    ("C-c C-." . idris-print-definition-of-name)
    ("C-c C-/" . idris-browse-namespace)
    ("M-q" . idris/mark-paragraph))

  (add-to-list 'core-popups-help-modes 'idris-info-mode :append)
  (add-to-list 'core-popups-help-modes 'idris-compiler-notes-mode :append))

(with-eval-after-load 'idris-hole-list
  (idris//setup-view-keys idris-hole-list-mode-map))

(with-eval-after-load 'idris-tree-info
  (idris//setup-view-keys idris-tree-info-mode-map))

(with-eval-after-load 'idris-info
  (idris//setup-view-keys idris-info-mode-map)

  (add-hook 'idris-info-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-function 'idris-eldoc-lookup))))

(provide 'init-idris)
