(with-eval-after-load 'semantic
  (semantic-add-system-include "/usr/include/c++/5/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    ;; global-semantic-idle-summary-mode
                                    global-semantic-decoration-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-mru-bookmark-mode)))

(provide 'init-semantic)
