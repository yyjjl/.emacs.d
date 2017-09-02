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

  (setq popwin:popup-window-width 0.33)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:popup-window-dedicated-p t)
  (setq popwin:special-display-config
        '(("*Warnings*" :noselect t)
          ("*info*" :width 0.5 :stick t)
          (dired-mode :width 30 :position left)
          (term-mode  :width 0.5 :stick t)
          ((lambda (mode) (derived-mode? 'comint-mode mode)) :width 0.5 :stick t)
          (help-mode :position bottom)
          (completion-list-mode :noselect t)
          (compilation-mode :noselect t)
          ;; Capture all other temp buffer
          ("^\\*.*?\\*" :regexp t :stick t)))
  (defun core*popup-auto-select-position (&rest $args)
    (setq popwin:popup-window-position
          (if (> (frame-width) split-width-threshold)
              'right
            'bottom))
    (setq popwin:popup-window-width
          (if (= (length (window-list-1)) 1) 0.5 0.33)))

  (advice-add 'popwin:display-buffer-1 :before
              (function core*popup-auto-select-position)))

(provide 'core-popups)
