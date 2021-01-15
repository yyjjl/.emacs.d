;; -*- lexical-binding:t -*-

(define-hook! ymacs-lisp//racket-setup (racket-mode-hook)
  (ymacs-lisp//common-setup)

  ;; (setq eldoc-documentation-function 'racket-eldoc-function)

  (racket-xp-mode 1)

  (unless (is-buffer-suitable-for-coding!)
    (setq completion-at-point-functions nil))

  ;; (flycheck-mode 1)

  (setq-local flycheck-check-syntax-automatically
              '(save mode-enabled))

  (set-local-minor-mode-map! 'lispy-mode
    (lispy-define-key the-map "e" #'ymacs-lisp/racket-eval-sexp)
    (lispy-define-key the-map "i" #'ymacs-lisp/racket-indent-sexp)))
