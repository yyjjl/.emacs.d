(eval-when-compile
  (require 'hydra))

(with-eval-after-load 'org
  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  _q_uote     _E_macs-lisp    _L_aTeX:
_l_atex   _e_xample   _C_pp           _i_ndex:
_a_scii   _v_erse     _I_NCLUDE:      _j_avascript
_s_rc     _S_hell     ^ ^             _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (org/hot-expand "<s"))
    ("e" (org/hot-expand "<e"))
    ("q" (org/hot-expand "<q"))
    ("v" (org/hot-expand "<v"))
    ("c" (org/hot-expand "<c"))
    ("l" (org/hot-expand "<l"))
    ("h" (org/hot-expand "<h"))
    ("a" (org/hot-expand "<a"))
    ("L" (org/hot-expand "<L"))
    ("i" (org/hot-expand "<i"))
    ("E" (org/hot-expand "<s" "emacs-lisp"))
    ("C" (org/hot-expand "<s" "cpp"))
    ("S" (org/hot-expand "<s" "sh"))
    ("j" (org/hot-expand "<s" "javascipt"))
    ("I" (org/hot-expand "<I"))
    ("H" (org/hot-expand "<H"))
    ("A" (org/hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun org/hot-expand ($str &optional $mod)
    "Expand org template."
    (insert $str)
    (org-try-structure-completion)
    (when $mod (insert $mod) (forward-line)))

  (defhydra hydra-org-move (:color pink :hint nil)
    "org move"
    ("u" outline-up-heading "up")              ; Up
    ("n" outline-next-visible-heading "next")  ; Next
    ("p" outline-previous-visible-heading "prev") ; Previous
    ("f" outline-forward-same-level "forward") ; Forward - same level
    ("b" outline-backward-same-level "back")   ; Backward - same level
    ("q" nil "quit")
    ("<tab>" org-cycle "cycle")
    ("RET" nil))

  (define-key! :map org-mode-map
    ("<" . (lambda () (interactive)
             (if (looking-back "^\\s-*" (line-beginning-position))
                 (hydra-org-template/body)
               (self-insert-command 1))))
    ("C-c C-u" . hydra-org-move/outline-up-heading)
    ("C-c C-n" . hydra-org-move/outline-next-visible-heading)
    ("C-c C-p" . hydra-org-move/outline-previous-visible-heading)
    ("C-c C-f" . hydra-org-move/outline-forward-same-level)
    ("C-c C-b" . hydra-org-move/outline-backward-same-level)))

(defhydra hydra-resize-window (:color pink)
  "shrink"
  ("{" shrink-window-horizontally "-><-")
  ("}" enlarge-window-horizontally "<-->")
  ("^" enlarge-window "enlarge")
  ("-" shrink-window "shrink")
  ("RET" nil "quit"))

(define-key!
  ("C-x {" . hydra-resize-window/shrink-window-horizontally)
  ("C-x }" . hydra-resize-window/enlarge-window-horizontally)
  ("C-x ^" . hydra-resize-window/enlarge-window)
  ("C-x -" . hydra-resize-window/shrink-window))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
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

(defhydra hydra-outline (:color pink :hint nil)
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

(global-set-key (kbd "C-c o") 'hydra-outline/body)

(defvar current-forward-thing 'char)
(defun current-forward-thing ()
  current-forward-thing)
(defun forward-thing* (&optional N)
  (interactive "p")
  (forward-thing current-forward-thing N))

(defun backward-thing* (&optional N)
  (interactive "p")
  (forward-thing current-forward-thing (when (numberp N) (- 0 N))))

(defhydra hydra-move (:color pink :hint nil)
  "
Current thing: %s(current-forward-thing)
[_p_/_n_] line      [_u_/_v_] scroll   [_b_/_f_] thing
[_a_/_e_] begin/end [_m_/_x_] mark     [_SPC_] scroll
[_l_] center      [,] change thing    [_q_] quit
"
  ("q" nil :exit t)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-thing*)
  ("b" backward-thing*)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  (", s" (setq current-forward-thing 'symbol))
  (", c" (setq current-forward-thing 'char))
  (", w" (setq current-forward-thing 'word))
  (", d" (setq current-forward-thing 'defun))
  (", l" (setq current-forward-thing 'list))
  ("v" scroll-up-command)
  ("SPC" scroll-up-command)
  ("u" scroll-down-command)
  ("m" set-mark-command)
  ("x" exchange-point-and-mark)
  ("l" recenter-top-bottom))

(defmacro core%define-use-other-window ($fn &optional $negative? $name)
  (unless $name (setq $name $fn))
  `(defun ,(intern (format "%s-other-window" $name))
       (&optional $N)
     (interactive "p")
     (when (= $N 0) (setq $N 1))
     ,(when $negative? '(setq $N (- $N)))
     (with-selected-window (next-window)
       (,$fn $N))))

(core%define-use-other-window forward-line)
(core%define-use-other-window forward-line :negative backward-line)

(defhydra hydra-move-other-window (:color pink :hint nil)
  "
[_p_/_n_] line      [_u_/_v_] scroll [_SPC_] scroll [_q_] quit
"
  ("q" nil :exit t)
  ("n" forward-line-other-window)
  ("p" backward-line-other-window)
  ("v" scroll-other-window)
  ("SPC" scroll-other-window)
  ("u" scroll-other-window-down))

(defun hydra-move-invoker (&optional $arg)
  (interactive "P")
  (if $arg
      (hydra-move-other-window/body)
    (hydra-move/body)))

(global-set-key (kbd "C-.") #'hydra-move-invoker)
(global-set-key (kbd "C-v") #'hydra-move/scroll-up-command)
(global-set-key (kbd "M-v") #'hydra-move/scroll-down-command)

(defhydra hydra-next-error (:color pink :hint nil)
  "
Compilation errors:
_n_: next error        _<_: first error
_p_: previous error
_q_uit
"
  ("`" next-error)
  ("n" next-error)
  ("p" previous-error)
  ("<" first-error)
  ("q" nil :color blue))
(global-set-key (kbd "M-g n") #'hydra-next-error/next-error)
(global-set-key (kbd "C-x `") #'hydra-next-error/next-error)
(global-set-key (kbd "M-g p") #'hydra-next-error/previous-error)

(provide 'core-hydra)
