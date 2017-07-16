
(setq flycheck-keymap-prefix (kbd "C-c f"))

(with-eval-after-load 'flycheck
  (setq-default flycheck-check-syntax-automatically
                '(idle-change save mode-enabled))

  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

  (setq flycheck-mode-line-prefix ""))


(provide 'init-flycheck)

;;; init-flycheck ends here