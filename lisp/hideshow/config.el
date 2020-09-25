;;; -*- lexical-binding: t; -*-

(after! hideshow
  (define-key! :map hs-minor-mode-map
    ("C-x t h" . hs-hide-block)
    ("C-x t s" . hs-show-block)
    ("C-x t H" . hs-hide-all)
    ("C-x t S" . hs-show-all)
    ("C-x t l" . hs-hide-level)
    ("C-x t t" . hs-toggle-hiding))

  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay 'ymacs-hideshow//abstract-overlay))
