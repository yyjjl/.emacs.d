;;; -*- lexical-binding: t; -*-

(require-packages!
 zone-sl
 zone-nyan
 zone-rainbow
 fireplace)



(defhydra hydra-select-games (:color red :hint nil)
  "
[_5_] 5x5            [_a_] animate      [_g_] gomoku    [_h_] hanoi   [_l_] life
[_m_] mpuz           [_p_] pong         [_t_] tetris    [_z_] zone    ^ ^
[_b 1_] blackbox     [_b 2_] bubbles    [_P_] pacmacs   ^ ^           ^ ^
[_s 1_] snake        [_s 2_] solitaire  ^ ^             ^ ^           ^ ^
[_d 1_] dissociated  [_d 2_] doctor     [_d 3_] dunnet  ^ ^           ^ ^
"
  ("5" 5x5)
  ("a" animate-birthday-present)
  ("b 1" blackbox)
  ("b 2" bubbles)
  ("d 1" dissociated-press)
  ("d 2" doctor)
  ("d 3" dunnet)
  ("g" gomoku)
  ("h" hanoi)
  ("l" life)
  ("m" mpuz)
  ("p" pong)
  ("P" pacmacs-start)
  ("s 1" snake)
  ("s 2" solitaire)
  ("t" tetris)
  ("z" zone))

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

(global-set-key (kbd "C-x , z") #'hydra-select-games/body)

(provide 'init-play)
