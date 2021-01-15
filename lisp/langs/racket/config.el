;;; -*- lexical-binding: t; -*-

(after! racket-mode
  (define-key! :map racket-mode-map
    ([f9] . racket-run)))
