(defun after-init-function ()
  (session-initialize)
  (turnon-keyfreq-mode)
  (workgroups-mode 1)
  (wg-reload-session)
  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)
  (fcitx-aggressive-setup))

(add-hook 'after-init-hook 'after-init-function)

(provide 'after-init)
