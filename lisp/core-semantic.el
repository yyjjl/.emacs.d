;; -*- lexical-binding: t; -*-

(config! semantic
  :advice
  (:override semantic-analyze-completion-at-point-function :name ignore)
  (:override semantic-analyze-notc-completion-at-point-function :name ignore)
  (:override semantic-analyze-nolongprefix-completion-at-point-function :name ignore)
  (:around semantic-new-buffer-fcn :name ignore-remote!)
  (:around semantic-idle-scheduler-function
   :define (-fn &rest -args) (with-local-quit (apply -fn -args)))

  :config
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

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

  (when emacs-use-gtags-p
    (dolist (mode '(c++-mode c-mode java-mode))
      (semanticdb-enable-gnu-global-databases mode))))

(provide 'core-semantic)
