(add-hook 'sh-mode-hook
          '(lambda ()
             (unless (buffer-temporary-p)
               (add-to-list 'company-backends 'company-files)
               (flycheck-mode 1))))


;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(provide 'init-sh)
