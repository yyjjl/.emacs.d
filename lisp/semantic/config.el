;; -*- lexical-binding: t; -*-

(after! semantic
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          ;; global-semantic-stickyfunc-mode
          ;; Error occurs a lot
          ;; global-semantic-decoration-mode
          ;; global-semantic-highlight-func-mode
          ;; global-semantic-mru-bookmark-mode
          ))
  (setq semantic-idle-scheduler-idle-time 1)

  (dolist (mode '(c++-mode c-mode java-mode))
    (semanticdb-enable-gnu-global-databases mode)))
