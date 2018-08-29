(with-eval-after-load 'semantic
  ;; (fset 'semantic-analyze-completion-at-point-function 'ignore)
  ;; (fset 'semantic-analyze-notc-completion-at-point-function 'ignore)
  ;; (fset 'semantic-analyze-nolongprefix-completion-at-point-function 'ignore)

  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (advice-add 'semantic-new-buffer-fcn :around #'ignore-remote!)

  (advice-add 'semantic-idle-scheduler-function
              :around (lambda (-fn &rest -args)
                        (with-local-quit (apply -fn -args))))

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          global-semantic-stickyfunc-mode
          ;; Error occurs a lot
          ;; global-semantic-decoration-mode
          ;; global-semantic-highlight-func-mode
          global-semantic-mru-bookmark-mode))
  (setq semantic-idle-scheduler-idle-time 1)

  (when env-has-gtags-p
    (dolist (mode '(c++-mode c-mode java-mode))
      (semanticdb-enable-gnu-global-databases mode))))

(add-hook 'after-init-hook 'semantic-mode)

(provide 'core-semantic)
