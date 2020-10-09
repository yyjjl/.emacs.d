;; -*- lexical-binding: t; -*-

(define-hook! ymacs-semantic|after-init (after-init-hook)
  (semantic-mode 1))

(after! semantic
  (advice-add #'semantic-analyze-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-notc-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-nolongprefix-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-new-buffer-fcn :around #'ignore-remote!)

  (define-advice semantic-idle-scheduler-function (:around (-fn &rest -args) allow-quit)
    (with-local-quit (apply -fn -args))))
