(add-hook 'sh-mode-hook
          (lambda ()
            (unless (buffer-temporary-p)
              (add-to-list 'company-backends 'company-files))))

(provide 'init-sh)
