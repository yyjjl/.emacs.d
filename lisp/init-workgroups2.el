(with-eval-after-load 'workgroups2
  (setq wg-prefix-key (kbd "C-c z"))
  (setq wg-mode-line-display-on nil)
  (setq wg-session-file (expand-file-name
                         "~/.emacs.d/data/workgroups"))
  (setq wg-load-last-workgroup nil))

(provide 'init-workgroups2)
