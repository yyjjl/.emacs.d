(with-eval-after-load 'workgroups2
  (setq wg-prefix-key (kbd "C-c z"))
  (setq wg-mode-line-display-on nil)
  (setq wg-session-file (expand-file-name
                         "~/.emacs.d/data/workgroups"))
  (setq wg-session-load-on-start nil)
  (setq wg-open-this-wg "none"))

(provide 'init-workgroups2)
