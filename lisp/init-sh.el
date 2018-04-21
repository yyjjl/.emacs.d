(add-hook 'sh-mode-hook
          (lambda ()
            (unless (buffer-temporary?)
              (add-to-list 'company-backends 'company-files))))

(provide 'init-sh)
