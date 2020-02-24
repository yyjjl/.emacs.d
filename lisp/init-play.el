;;; -*- lexical-binding: t; -*-

(require-packages!
 zone-sl
 zone-nyan
 zone-rainbow
 fireplace)

(with-eval-after-load 'zone
  (setq zone-programs
        (vconcat zone-programs [zone-pgm-sl zone-pgm-rainbow zone-nyan]))
  (defun zone*around-fullscreen (-fn)
    (let ((buffer (get-buffer-create "*zone*")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil))))
    (save-window-excursion
      (let ((fullscreen-p (memq (frame-parameter nil 'fullscreen)
                                '(fullscreen fullboth))))
        (unless fullscreen-p (toggle-frame-fullscreen))
        (delete-other-windows)
        (unwind-protect
            (progn
              (funcall -fn)
              (when (input-pending-p)
                (discard-input)))
          (unless fullscreen-p (toggle-frame-fullscreen))))))
  (advice-add 'zone :around #'zone*around-fullscreen))

(with-eval-after-load 'life
  (add-hook 'life-mode-hook
            (lambda ()
              (setq-local header-line-format nil)
              (setq-local mode-line-format '(:eval (mode-line//buffer-id))))))

(with-eval-after-load 'zone-nyan
  (setq zone-nyan-text-size (/ (frame-width) 2))
  (setq zone-nyan-gui-type 'text)
  (setq zone-nyan-term-type 'ascii))

(autoload 'hydra-select-games/body "autoloads/play" nil t)
(global-set-key (kbd "C-x , z") #'hydra-select-games/body)

(provide 'init-play)
