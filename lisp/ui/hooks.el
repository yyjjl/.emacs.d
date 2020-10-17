;;; -*- lexical-binding: t; -*-

;; Setup `mode-line-format'
(define-hook! ymacs-ui|setup-modeline (after-init-hook)
  (winum-mode 1)
  (doom-modeline-mode 1)

  (setq-default mode-line-buffer-identification '("%b")))

(define-hook! ymacs-ui|setup-headerline (prog-mode-hook text-mode-hook)
  (setq header-line-format '(:eval (doom-modeline-segment--misc-info))))
