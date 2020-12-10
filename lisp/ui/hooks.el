;;; -*- lexical-binding: t; -*-

;; Setup `mode-line-format'
(define-hook! ymacs-ui|setup-modeline (after-init-hook)
  (winum-mode 1)
  (doom-modeline-mode 1)

  (global-font-lock-mode 1)
  (global-page-break-lines-mode 1)
  (global-hl-todo-mode 1)

  (global-whitespace-mode 1)

  ;; (which-function-mode 1)
  (which-key-mode 1)

  (column-number-mode 1)
  (show-paren-mode 1)

  (setq-default mode-line-buffer-identification '("%b")))

(define-hook! ymacs-ui|setup-headerline (prog-mode-hook text-mode-hook)
  (setq header-line-format '(:eval (list
                                    (doom-modeline-segment--debug)
                                    (doom-modeline-segment--misc-info)))))
