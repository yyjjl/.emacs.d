(require 'hydra)

(setq hydra-lv nil)

(with-eval-after-load 'org
  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  _q_uote     _E_macs-lisp    _L_aTeX:
_l_atex   _e_xample   _C_pp           _i_ndex:
_a_scii   _v_erse     _I_NCLUDE:      _j_avascript
_s_rc     _S_hell     ^ ^             _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("E" (hot-expand "<s" "emacs-lisp"))
    ("C" (hot-expand "<s" "cpp"))
    ("S" (hot-expand "<s" "sh"))
    ("j" (hot-expand "<s" "javascipt"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun hot-expand (str &optional mod)
    "Expand org template."
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line)))

  (bind-key "<" (lambda () (interactive)
                  (if (looking-back "^\\s-*")
                      (hydra-org-template/body)
                    (self-insert-command 1))) org-mode-map))

(defhydra hydra-move ()
  "move"
  ("n" next-line )
  ("p" previous-line )
  ("f" forward-char )
  ("b" backward-char )
  ("v" scroll-up-command )
  ;; Converting M-v to V here by analogy .
  ("V" scroll-down-command )
  ("l" recenter-top-bottom)

  ("C-SPC" set-mark-command)
  ("x" exchange-point-and-mark)
  ("RET" nil nil))

(bind-keys ("C-n" . hydra-move/next-line)
           ("C-p" . hydra-move/previous-line)
           ("C-b" . hydra-move/backward-char)
           ("C-f" . hydra-move/forward-char)
           ("C-SPC" . hydra-move/set-mark-command))

;; hydra move make mc too slow
(with-eval-after-load 'multiple-cursors-core
  (bind-keys :map mc/keymap
             ("C-n" . next-line)
             ("C-p" . previous-line)
             ("C-f" . forward-char)
             ("C-b" . backward-char)))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_p_^     [_d_]delete    [_s_]string
_b_   _f_   [_o_]ok        [_y_]yank
  ^_n_^     [_r_]reset     [_w_]cut
^^^^        [_x_]exchange  [_/_]undo
^^^^        [_a_]line beg  [_e_]line end
"
  ("b" backward-char nil)
  ("f" forward-char nil)
  ("p" previous-line nil)
  ("n" next-line nil)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("x" exchange-point-and-mark nil)
  ("w" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("/" undo nil)
  ("s" string-rectangle nil)
  ("k" kill-rectangle nil)
  ("o" nil nil))
(define-key global-map (kbd "C-x SPC") 'hydra-rectangle/body)

(provide 'init-hydra)