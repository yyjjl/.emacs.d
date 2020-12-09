;; -*- lexical-binding:t -*-

(define-hook! ymacs-lisp|racket-setup (racket-mode-hook)
  (ymacs-lisp|common-setup)
  ;; (setq eldoc-documentation-function 'racket-eldoc-function)
  (unless (buffer-enable-rich-feature-p)
    (setq completion-at-point-functions nil))
  (flycheck-mode 1)

  (setq-local flycheck-check-syntax-automatically
              '(save mode-enabled))

  (with-local-minor-mode-map! 'lispy-mode
    (lispy-define-key it "e" #'ymacs-lisp/racket-eval-sexp)
    (lispy-define-key it "i" #'ymacs-lisp/racket-indent-sexp)))
