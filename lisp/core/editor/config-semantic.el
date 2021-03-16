;;; -*- lexical-binding: t; -*-

(after! semantic
  (advice-add #'semantic-analyze-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-notc-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-nolongprefix-completion-at-point-function :override #'ignore)

  (define-hook! ymacs-semantic//inhibit-function (semantic-inhibit-functions)
    (or (and default-directory (file-remote-p default-directory))
        (not (derived-mode-p 'prog-mode))))

  (define-advice semantic-idle-scheduler-function (:around (-fn &rest -args) allow-quit)
    (with-local-quit (apply -fn -args)))

  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (define-key! :map semantic-mode-map :prefix "C-c ,"
    ("." . semantic-ia-fast-jump)
    ("v" . semantic-ia-show-variants)
    (("TAB" "<tab>") . semantic-ia-complete-symbol))

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(;; global-semantic-idle-scheduler-mode
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
  (setq semanticdb-project-root-functions '(ymacs-editor//project-root))

  (dolist (mode '(c++-mode c-mode java-mode))
    (semanticdb-enable-gnu-global-databases mode)))