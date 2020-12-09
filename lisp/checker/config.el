;; -*- lexical-binding: t; -*-

(custom-set-variables
 '(flycheck-keymap-prefix (kbd "C-c f")))

(after! flycheck
  (define-key! :map flycheck-command-map
    ("j" . ymacs-checker/jump-error))

  ;; Do not check during newline
  (setq-default flycheck-checker-error-threshold 400)
  (setq-default flycheck-check-syntax-automatically '(idle-change save mode-enabled))
  (setq flycheck-navigation-minimum-level 'warning)
  (setq flycheck-mode-line-prefix "")
  (setq flycheck-idle-change-delay 1))
