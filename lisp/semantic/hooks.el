;; -*- lexical-binding: t; -*-

(define-hook! ymacs-semantic|after-init (after-init-hook)
  (semantic-mode 1))

(after! semantic
  (advice-add #'semantic-analyze-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-notc-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-nolongprefix-completion-at-point-function :override #'ignore)

  (define-hook! ymacs-semantic|inhibit-function (semantic-inhibit-functions)
    (or (and default-directory (file-remote-p default-directory))
        (not (derived-mode-p 'prog-mode))))

  (define-advice semantic-idle-scheduler-function (:around (-fn &rest -args) allow-quit)
    (with-local-quit (apply -fn -args))))
