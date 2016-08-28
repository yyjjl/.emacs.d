(defun after-init-function ()
  (session-initialize)
  (turnon-keyfreq-mode)
  (workgroups-mode 1)
  (wg-reload-session)

  (ivy-mode 1)
  (counsel-mode 1)

  (guide-key-mode 1)  ;; Enable guide-key-mode

  (popwin-mode 1)  ;; Enable popwin-mode

  (global-git-gutter-mode 1)

  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)
  (fcitx-aggressive-setup)

  (message "Emacs setup time: %s" (emacs-init-time)))

(add-hook 'after-init-hook 'after-init-function)

(provide 'after-init)
