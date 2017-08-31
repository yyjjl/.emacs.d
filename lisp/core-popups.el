(defun core/popup-dired (dir)
  (interactive (list default-directory))
  (popwin:display-buffer (dired-noselect dir))
  (dired%add-window (selected-window))
  (popwin:stop-close-popup-window-timer))

(defun core/popup-dired-root ()
  (interactive)
  (core/popup-dired (projectile-project-root)))

(autoload 'popwin-mode "popwin" nil t)
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap)
  (global-set-key (kbd "C-x m") #'popwin:messages)
  (define-key! :map popwin:keymap
    ("d" . core/popup-dired)
    ("r" . core/popup-dired-root))

  (setq popwin:popup-window-width 0.4)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:popup-window-dedicated-p t)
  (setq popwin:special-display-config
        '("*Backtrace*"
          ("*Warnings*" :noselect t)
          ("*info*" :stick t :width 0.5)
          (dired-mode :width 30 :position left)
          (term-mode :stick t :width 0.5)
          ((lambda (mode) (derived-mode? 'comint-mode mode)) :width 0.5 :stick t)
          (help-mode :position bottom)
          (completion-list-mode :noselect t)
          (compilation-mode :noselect t)
          (grep-mode :select t)
          (occur-mode :stick t)
          ("^\\*ivy-occur .*" :regexp t :stick t)
          ("^\\*magit:.*" :regexp t :stick t)
          ;; Capture all other temp buffer
          ("^\\*.*?\\*" :regexp t)))
  (defun core*popup-auto-select-psotion (&rest $args)
    (setq popwin:popup-window-position
          (if (> (frame-width) split-width-threshold)
              'right
            'bottom)))
  (advice-add 'popwin:display-buffer-1 :before
              (function core*popup-auto-select-psotion)))

(provide 'core-popups)
