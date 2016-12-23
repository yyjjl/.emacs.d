(defun after-init-function ()
  (session-initialize)
  (turnon-keyfreq-mode)

  (ivy-mode 1)
  (counsel-mode 1)

  (guide-key-mode 1)  ;; Enable guide-key-mode

  (popwin-mode 1)  ;; Enable popwin-mode

  (global-git-gutter-mode 1)
;;  (git-gutter:linum-setup)

  (global-linum-mode 1)

  (global-company-mode 1)

  (global-flycheck-mode 1)

  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)
  (fcitx-aggressive-setup)

  ;; may take a long time
  (elpy-enable)

  (workgroups-mode 1)
  (winner-mode 1)
  (wg-reload-session)
  (message "Emacs setup time: %s" (emacs-init-time)))

(add-hook 'after-init-hook 'after-init-function)

(provide 'after-init)
