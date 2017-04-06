(with-eval-after-load 'semantic
  (semantic-add-system-include "/usr/include/c++/5/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    ;; global-semantic-idle-summary-mode
                                    global-semantic-stickyfunc-mode
                                    global-semantic-decoration-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-mru-bookmark-mode))
  (require 'stickyfunc-enhance))

(defun try-turn-on-semantic-mode ()
  (if (> (buffer-size) large-buffer-size)
      (semantic-mode -1)
    (semantic-mode 1)))

(provide 'init-semantic)
