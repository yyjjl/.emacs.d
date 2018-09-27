;;; -*- lexical-binding: t; -*-

(add-hook 'sh-mode-hook
          (lambda ()
            (unless (buffer-temporary-p)
              (flycheck-mode 1)
              (add-to-list 'company-backends 'company-files))))

(provide 'init-sh)
