
(setq flycheck-keymap-prefix (kbd "C-c f"))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-html-tidy-executable
        (expand-file-name "~/.emacs.d/bin/tidy"))
  (setq flycheck-mode-line-prefix "")
  (flycheck-pos-tip-mode 1))


(provide 'init-flycheck)

;;; init-flycheck ends here