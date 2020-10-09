;;; -*- lexical-binding: t; -*-

(defhydra ymacs-hydra/select-games (:color blue :hint nil)
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
