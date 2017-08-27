
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
  (setq popwin:popup-window-width 0.3)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:special-display-config
        '("*Backtrace*"
         ("*Warnings*" :noselect t)
         ("*info*" :stick t)
         (term-mode :stick t :width 0.5)
         (dired-mode :width 30 :position left)
         ("*Miniedit Help*" :noselect t)
         (help-mode :position bottom)
         (completion-list-mode :noselect t)
         (compilation-mode :noselect t)
         (grep-mode :noselect t)
         (occur-mode :noselect t)
         ("*Pp Macroexpand Output*" :noselect t)
         "*Shell Command Output*"
         "*vc-diff*"
         "*vc-change-log*"
         ("^\\*anything.*\\*$" :regexp t)))
  (defun core*popup-auto-select-psotion (&rest $args)
    (setq popwin:popup-window-position
          (if (> (frame-width) split-width-threshold)
              'right
            'bottom)))
  (advice-add 'popwin:popup-buffer :before
              (function core*popup-auto-select-psotion)))

(provide 'core-popups)
