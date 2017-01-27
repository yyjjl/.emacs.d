(require 'hydra)

;; (setq hydra-lv nil)

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

  (defhydra hydra-org-move (:color pink :hint nil)
    "org move"
    ("u" outline-up-heading "up")                 ; Up
    ("n" outline-next-visible-heading "next")     ; Next
    ("p" outline-previous-visible-heading "prev") ; Previous
    ("f" outline-forward-same-level "forward") ; Forward - same level
    ("b" outline-backward-same-level "back")   ; Backward - same level
    ("q" nil "quit")
    ("<tab>" yas-expand)
    ("RET" nil))

  (bind-keys :map org-mode-map
             ("<" . (lambda () (interactive)
                      (if (looking-back "^\\s-*")
                          (hydra-org-template/body)
                        (self-insert-command 1))))
             ("C-c u" . hydra-org-move/outline-up-heading)
             ("C-c n" . hydra-org-move/outline-next-visible-heading)
             ("C-c p" . hydra-org-move/outline-previous-visible-heading)
             ("C-c f" . hydra-org-move/outline-forward-same-level)
             ("C-c b" . hydra-org-move/outline-backward-same-level)))

(defhydra hydra-resize-window (:color pink)
  "shrink"
  ("{" shrink-window-horizontally "-><-")
  ("}" enlarge-window-horizontally "<-->")
  ("^" enlarge-window "enlarge")
  ("-" shrink-window  "shrink")
  ("RET" nil "quit"))

(bind-keys ("C-x {" . hydra-resize-window/shrink-window-horizontally)
           ("C-x }" . hydra-resize-window/enlarge-window-horizontally)
           ("C-x ^" . hydra-resize-window/enlarge-window)
           ("C-x _" . hydra-resize-window/shrink-window))


(define-key global-map (kbd "C-x SPC") 'hydra-rectangle/body)

(defhydra hydra-rectangle (:color pink)
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
  ("a" beginning-of-line nil)
  ("e" end-of-line nil)
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