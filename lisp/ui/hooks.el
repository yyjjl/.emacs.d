;;; -*- lexical-binding: t; -*-

;; Setup `mode-line-format'
(define-hook! ymacs-ui|setup-modeline (after-init-hook)
  (winum-mode 1)
  (doom-modeline-mode 1)

  (setq-default header-line-format
                '(:eval (doom-modeline-segment--misc-info)))

  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default mode-line-misc-info
                '((company-search-mode (" " company-search-lighter))
                  (global-mode-string (" " global-mode-string)))))
