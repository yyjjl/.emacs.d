
(defun core/popup-dired (dir)
  (interactive (list default-directory))
  (popwin:display-buffer (dired-noselect dir)))

(defun core/popup-dired-root ()
  (interactive)
  (core/popup-dired (projectile-project-root)))

(autoload 'popwin-mode "popwin" nil t)
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap)
  (define-key! :map popwin:keymap
    ("d" . core/popup-dired)
    ("r" . core/popup-dired-root))
  (setq popwin:popup-window-width 0.4)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:special-display-config
        (append
         '(("*Backtrace*" :width 0.5)
           ("*Warnings*" :noselect t)
           ("*info*" :width 0.5 :stick t)
           (term-mode :stick t :width 0.5)
           (dired-mode :width 0.25 :position left))
         popwin:special-display-config))
  (defun core*popup-auto-select-psotion (&rest $args)
    (setq popwin:popup-window-position
          (if (> (frame-width) split-width-threshold)
              'right
            'bottom)))
  (advice-add 'popwin:popup-buffer :before
              (function core*popup-auto-select-psotion)))

(provide 'core-popups)
