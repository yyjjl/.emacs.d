;;; -*- lexical-binding: t; -*-

(after! iedit
  (setq iedit-auto-narrow t))

(after! picture
  (define-key! :map picture-mode-map
    ("C-c C-a" . artist-mode)))
