
(with-eval-after-load 'hippie-expand
    (setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
    (global-set-key (kbd "M-/") 'hippie-expand))

(provide 'init-hippie-expand)
