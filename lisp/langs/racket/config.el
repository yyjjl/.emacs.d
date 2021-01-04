;;; -*- lexical-binding: t; -*-

(after! racket-mode
  (define-key! :map racket-mode-map
    ([f5] . racket-run-with-debugging)
    ([f9] . racket-run)))
