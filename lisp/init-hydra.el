(require 'hydra)

(with-eval-after-load 'org
  (defhydra hydra|org-template (:color blue :hint nil)
    "
_c_enter  _q_uote     _E_macs-lisp    _L_aTeX:
_l_atex   _e_xample   _C_pp           _i_ndex:
_a_scii   _v_erse     _I_NCLUDE:      _j_avascript
_s_rc     _S_hell     ^ ^             _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (org|hot-expand "<s"))
    ("e" (org|hot-expand "<e"))
    ("q" (org|hot-expand "<q"))
    ("v" (org|hot-expand "<v"))
    ("c" (org|hot-expand "<c"))
    ("l" (org|hot-expand "<l"))
    ("h" (org|hot-expand "<h"))
    ("a" (org|hot-expand "<a"))
    ("L" (org|hot-expand "<L"))
    ("i" (org|hot-expand "<i"))
    ("E" (org|hot-expand "<s" "emacs-lisp"))
    ("C" (org|hot-expand "<s" "cpp"))
    ("S" (org|hot-expand "<s" "sh"))
    ("j" (org|hot-expand "<s" "javascipt"))
    ("I" (org|hot-expand "<I"))
    ("H" (org|hot-expand "<H"))
    ("A" (org|hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun org|hot-expand (str &optional mod)
    "Expand org template."
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line)))

   (defhydra hydra|org-move (:color pink :hint nil)
    "org move"
    ("u" outline-up-heading "up")              ; Up
    ("n" outline-next-visible-heading "next")  ; Next
    ("p" outline-previous-visible-heading "prev") ; Previous
    ("f" outline-forward-same-level "forward") ; Forward - same level
    ("b" outline-backward-same-level "back")   ; Backward - same level
    ("q" nil "quit")
    ("<tab>" org-cycle "cycle")
    ("RET" nil))

  (define-keys :map org-mode-map
    ("<" . (lambda () (interactive)
             (if (looking-back "^\\s-*")
                 (hydra|org-template/body)
               (self-insert-command 1))))
    ("C-c C-u" . hydra|org-move/outline-up-heading)
    ("C-c C-n" . hydra|org-move/outline-next-visible-heading)
    ("C-c C-p" . hydra|org-move/outline-previous-visible-heading)
    ("C-c C-f" . hydra|org-move/outline-forward-same-level)
    ("C-c C-b" . hydra|org-move/outline-backward-same-level)))

(defhydra hydra|resize-window (:color pink)
  "shrink"
  ("{" shrink-window-horizontally "-><-")
  ("}" enlarge-window-horizontally "<-->")
  ("^" enlarge-window "enlarge")
  ("-" shrink-window "shrink")
  ("RET" nil "quit"))

(define-keys
  ("C-x {" . hydra|resize-window/shrink-window-horizontally)
  ("C-x }" . hydra|resize-window/enlarge-window-horizontally)
  ("C-x ^" . hydra|resize-window/enlarge-window)
  ("C-x _" . hydra|resize-window/shrink-window))

(defhydra hydra|rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
  ^_p_^         [_k_]kill      [_s_]string
_b_   _f_       [_q_]quit      [_y_]yank
  ^_n_^         [_m_]mark      [_w_]copy
^^^^            [_x_]exchange  [_/_]undo
^^^^            [_a_]line beg  [_e_]line end [_N_]umber
"
  ("b" backward-char nil)
  ("f" forward-char nil)
  ("p" previous-line nil)
  ("n" next-line nil)
  ("a" beginning-of-line nil)
  ("e" end-of-line nil)
  ("x" rectangle-exchange-point-and-mark nil)
  ("w" copy-rectangle-as-kill nil)
  ("m" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1))
   nil)
  ("y" yank-rectangle nil)
  ("/" undo nil)
  ("N" rectangle-number-lines nil :exit t)
  ("s" string-rectangle nil)
  ("k" kill-rectangle nil)
  ("q" nil nil))
(define-key global-map (kbd "C-x SPC") 'hydra|rectangle/body)

(with-eval-after-load 'ivy
  (defhydra hydra|ivy (:hint nil :color pink)
    "
^^^^^^          ^Actions^       ^Quit^
^^^^^^--------------------------------------------
^ ^ _p_ ^ ^     _SPC_ repeat     _c_ancel
_<_ ^✜^ _>_     _q_uit
^ ^ _n_ ^ ^
"
    ;; arrows
    ("<" ivy-beginning-of-buffer)
    ("n" ivy-next-line)
    ("p" ivy-previous-line)
    (">" ivy-end-of-buffer)
    ;; actions
    ("SPC" hydra-repeat)
    ;; exit
    ("q" keyboard-escape-quit :exit t)
    ("c" nil))

  (define-key ivy-minibuffer-map (kbd "C-j") 'hydra|ivy/body))

(defhydra hydra|outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" outline-hide-sublevels) ; Hide everything but the top-level headings
  ("t" outline-hide-body) ; Hide everything but headings (all body lines)
  ("o" outline-hide-other)              ; Hide other branches
  ("c" outline-hide-entry)              ; Hide this entry's body
  ("l" outline-hide-leaves) ; Hide body lines in this entry and sub-entries
  ("d" outline-hide-subtree) ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" outline-show-all)                ; Show (expand) everything
  ("e" outline-show-entry)              ; Show this heading's body
  ("i" outline-show-children) ; Show this heading's immediate child sub-headings
  ("k" outline-show-branches) ; Show all sub-headings under this heading
  ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)              ; Up
  ("n" outline-next-visible-heading)    ; Next
  ("p" outline-previous-visible-heading) ; Previous
  ("f" outline-forward-same-level)       ; Forward - same level
  ("b" outline-backward-same-level)      ; Backward - same level
  ("z" nil "leave"))

(global-set-key (kbd "C-c o") 'hydra|outline/body) ; by example

(provide 'init-hydra)
