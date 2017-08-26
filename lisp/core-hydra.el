(require 'hydra)

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

(global-set-key (kbd "C-c o") 'hydra-outline/body) ; by example

(defvar current-forward-thing 'char)
(defun current-forward-thing ()
  current-forward-thing)
(defun forward-thing* (&optional N)
  (interactive "p")
  (forward-thing current-forward-thing N))

(defun backward-thing* (&optional N)
  (interactive "p")
  (forward-thing current-forward-thing (when (numberp N) (- 0 N))))


(defhydra hydra-move (:pre (setq current-forward-thing 'char)
                           :color pink
                           :hint nil)
  "
Current thing: %s(current-forward-thing)
[_p_/_n_] line     [_u_/_v_] scroll   [_b_/_f_] thing
[_(_/_)_] sexp     [_a_/_e_] line begin/end
[_[_/_]_] page     [_{_/_}_] paragraph
[_SPC_/_x_] mark   [_l_] center     [,] change thing    [_q_] quit
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
  ("u" scroll-down-command)
  ("(" backward-sexp)
  ("[" backward-page)
  ("{" backward-paragraph)
  ("]" forward-page)
  ("}" forward-paragraph)
  (")" forward-sexp)
  ("SPC" set-mark-command)
  ("x" exchange-point-and-mark)
  ("l" recenter-top-bottom))

(global-set-key (kbd "C-.") #'hydra-move/body)
(global-set-key (kbd "C-v") #'hydra-move/scroll-up-command)
(global-set-key (kbd "M-v") #'hydra-move/scroll-down-command)

(defhydra hydra-next-error (global-map "C-x")
  "
Compilation errors:
_n_: next error        _<_: first error
_p_: previous error    _q_uit
"
  ("`" next-error     nil)
  ("n" next-error     nil :bind nil)
  ("p" previous-error nil :bind nil)
  ("<" first-error    nil :bind nil)
  ("q" nil            nil :color blue))
(global-set-key (kbd "M-g n") #'hydra-next-error/next-error)
(global-set-key (kbd "M-g p") #'hydra-next-error/previous-error)

(defvar x-hydra-timer nil)
(defvar x-hydra-delay 0.5)
(defvar x-hydra-last-prefix nil)
(defvar-local x-hydra-last-buffer-undo-list nil)
(defun x-hydra-quit (&optional no-hydra-quit)
  (when x-hydra-timer
    (cancel-timer x-hydra-timer)
    (setq x-hydra-timer nil))
  (unless no-hydra-quit
    (hydra-keyboard-quit)))

(defun x-hydra-pre ()
  (setq x-hydra-timer (timer-create))
  (setq x-hydra-last-buffer-undo-list buffer-undo-list)
  (hydra-set-property 'x-hydra :verbosity 0)
  (unless buffer-read-only (insert "x"))
  (timer-set-time x-hydra-timer
                  (timer-relative-time (current-time) x-hydra-delay))
  (timer-set-function x-hydra-timer 'x-hydra-quit)
  (timer-activate x-hydra-timer))

(defun x-hydra-dispatch (char)
  (interactive "c")
  (setq char (- char 96))
  (let ((cmd (if x-hydra-last-prefix
                 (list x-hydra-last-prefix char)
               (list char))))
    (setq unread-command-events cmd))
  (setq x-hydra-last-prefix nil))

(defmacro x-hydra-lambda-body (char)
  `(progn
     (setq x-hydra-last-prefix ,char)
     (ignore-errors
       (let ((inhibit-message t))
         (undo)
         (setq buffer-undo-list x-hydra-last-buffer-undo-list)))
     (x-hydra-quit t)
     (setq unread-command-events (list ,char))))

(defhydra x-hydra (:body-pre x-hydra-pre
                             :color blue)
  ("x" (x-hydra-lambda-body ?\C-x))
  ("c" (x-hydra-lambda-body ?\C-c))
  ("z" (x-hydra-lambda-body ?\C-z))
  ("h" (x-hydra-lambda-body ?\C-h)))

(setq x-hydra/keymap
      (define-key! :map (make-sparse-keymap)
        ("x" . x-hydra/lambda-x-and-exit)
        ("c" . x-hydra/lambda-c-and-exit)
        ("z" . x-hydra/lambda-z-and-exit)
        ("h" . x-hydra/lambda-h-and-exit)))

(global-set-key "x" #'x-hydra/body)
(global-set-key (kbd "C-x x") #'x-hydra-dispatch)

(provide 'core-hydra)
