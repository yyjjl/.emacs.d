(add-hook 'sh-mode-hook
          '(lambda ()
             (add-to-list 'company-backends 'company-shell)
             (flycheck-mode 1)))


;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda()
          (concat (getenv "USER") "@" (getenv "HOST") ":"
                  (eshell/pwdx)
                  (if (= (user-uid) 0) " # " " $ ")))))

(provide 'init-sh)
