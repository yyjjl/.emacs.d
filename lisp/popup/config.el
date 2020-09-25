;;  -*- lexical-binding: t -*-

(after! shackle
  (define-key!
    ("C-z" :map ymacs-ctrl-z-map))

  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-default-rule nil
        shackle-rules
        `(((:custom ymacs-popups//comint-buffer-p)
           :align below :select t :dedicated t)
          ((:custom ymacs-popups//help-buffer-p)
           :align below :select t :autoclose t)
          (,ymacs-popups-other-window-regexp :regexp t :select t :autoclose t)
          (occur-mode :select t)
          ("*Flycheck error messages*" :align below :select nil :size 0.3)
          ("*Warnings*" :align below :autoclose t)
          (,ymacs-popups-default-regexp :regexp t))))
