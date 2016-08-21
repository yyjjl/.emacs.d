(autoload 'jde-mode "jde" "JDE mode" t)
(setq auto-mode-alist
      (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))

(add-hook 'jde-mode-hook
          '(lambda ()
             (local-set-key (kbd "<backtab>") 'jde-complete)))

(provide 'init-java)