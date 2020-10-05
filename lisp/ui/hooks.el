;;; -*- lexical-binding: t; -*-

(define-advice doom-modeline-def-modeline
    (:override (-name -lhs &optional -rhs) left-align)
  (let ((sym (intern (format "doom-modeline-format--%s" -name)))
        (lhs-forms (doom-modeline--prepare-segments -lhs))
        (rhs-forms (doom-modeline--prepare-segments -rhs)))
    (defalias sym (lambda () (list lhs-forms rhs-forms)))))

;; Setup `mode-line-format'
(define-hook! ymacs-ui|setup-modeline (after-init-hook)
  (winum-mode 1)
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default mode-line-misc-info
                '((company-search-mode (" " company-search-lighter))
                  " " global-mode-string)))
