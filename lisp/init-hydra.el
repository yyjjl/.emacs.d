(require 'hydra)

(with-eval-after-load 'ivy
  (defun ivy--matcher-desc ()
    (if (eq ivy--regex-function
            'ivy--regex-fuzzy)
        "fuzzy"
      "ivy"))

  (defhydra hydra-ivy (:hint nil
                             :color pink)
    "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _p_ ^ ^ | _f_ollow occ_u_r | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_r_ ^+^ _l_ | _d_one      ^ ^  | _o_ops   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
^ ^ _n_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
"
    ;; arrows
    ("r" ivy-beginning-of-buffer)
    ("n" ivy-next-line)
    ("p" ivy-previous-line)
    ("l" ivy-end-of-buffer)
    ;; actions
    ("o" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("i" nil)
    ("C-o" nil)
    ("f" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("d" ivy-done :exit t)
    ("g" ivy-call)
    ("C-m" ivy-done :exit t)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("u" ivy-occur :exit t)
    ("D" (ivy-exit-with-action
          (lambda (_) (find-function 'hydra-ivy/body)))
     :exit t)))

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


(provide 'init-hydra)