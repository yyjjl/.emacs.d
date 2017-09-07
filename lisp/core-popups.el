(defun core/popwin-fix-window ()
  (interactive)
  (when (window-live-p popwin:popup-window)
    (message "Set %s to normal window" popwin:popup-window)
    (setq popwin:popup-window nil)
    (popwin:pop-context)))

(autoload 'popwin-mode "popwin" nil t)
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap)
  (global-set-key (kbd "C-x m") #'popwin:messages)
  (define-key! :map popwin:keymap
    ("RET" . core/popwin-fix-window))

  (setq popwin:popup-window-width 0.4)
  (setq popwin:popup-window-height 0.4)
  (setq popwin:popup-window-dedicated-p t)
  (setq popwin:special-display-config
        '(("*Warnings*" :noselect t)
          ("*sdcv*" :position bottom)
          ((lambda (buffer)
             (or (derived-mode? 'comint-mode buffer)
                 (eq (buffer-local-value 'major-mode buffer) 'term-mode)))
           :position bottom :stick t)
          (help-mode :position bottom)
          (completion-list-mode :noselect t)
          (compilation-mode :noselect t)
          ;; Capture all other temp buffer
          ("^\\*.*?\\*" :regexp t :stick t)))

  (defun core*popup-auto-select-position (&rest $args)
    (let* ((root (car (window-tree)))
           (num (if (or (atom root) (car-safe root))
                    2
                  (length (cdr root)))))
      (setq popwin:popup-window-width (/ 1.0 num)))
    (setq popwin:popup-window-position
          (if (> (frame-width) split-width-threshold)
              'right
            'bottom)))

  (advice-add 'popwin:display-buffer-1 :before
              (function core*popup-auto-select-position)))

(provide 'core-popups)
