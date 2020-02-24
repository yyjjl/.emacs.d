;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rect)
  (require 'hydra))

(require 'pretty-hydra)

;;;###autoload
(defvar hydra-local-toggles-heads-list nil)

(pretty-hydra-define hydra-global-toggles
  (:title "Toggles" :color amaranth :quit-key "q")
  ("Global"
   (("v" (core-view-code-mode (if core-view-code-mode -1 1))
     "view code" :toggle core-view-code-mode)
    ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    ("W" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
     "whitespace" :toggle show-trailing-whitespace)
    ("N" (display-line-numbers-mode (if display-line-numbers-mode -1 1))
     "line number" :toggle display-line-numbers-mode))))

(defun hydra-global-toggles/update ()
  (eval
   (pretty-hydra--generate
    'hydra-global-toggles
    '(:title "Toggles" :color amaranth :quit-key "q")
    (pretty-hydra--normalize-heads-plist!
     (append `("Global" ,(lax-plist-get hydra-global-toggles/heads-plist "Global"))
             (cl-loop for (key . value) in hydra-local-toggles-heads-list
                      when (eval key) append value)))))
  (define-key hydra-global-toggles/keymap
    (where-is-internal 'hydra-global-toggles/body nil t) #'hydra-global-toggles/nil)
  (prog1 (eval hydra-global-toggles/hint)
    (setq hydra-global-toggles/hint hydra-global-toggles/hint-cache)))

;; add additional columns dynamicly
(setq hydra-global-toggles/hint-cache '(hydra-global-toggles/update))
(setq hydra-global-toggles/hint hydra-global-toggles/hint-cache)

(defhydra hydra-sort (:color red)
  "sort"
  ("r" sort-regexp-fields "regexp")
  ("f" sort-fields "fields")
  ("c" sort-columns "columns")
  ("l" sort-lines "lines")
  ("n" sort-numeric-fields "numeric")
  ("F" flush-lines "flush")
  ("k" keep-lines "keep")
  ("RET" nil "quit")
  ("q" nil "quit"))

(defhydra hydra-resize-window (:color pink)
  "shrink"
  ("{" shrink-window-horizontally "-><-")
  ("}" enlarge-window-horizontally "<-->")
  ("^" enlarge-window "enlarge")
  ("-" shrink-window "shrink")
  ("RET" nil "quit"))

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

(defvar current-forward-thing 'char)

(defun current-forward-thing ()
  current-forward-thing)

(defun forward-thing* (&optional -n)
  (interactive "p")
  (forward-thing current-forward-thing -n))

(defun backward-thing* (&optional -n)
  (interactive "p")
  (forward-thing current-forward-thing (when (numberp -n) (- 0 -n))))

(defhydra hydra-move (:color pink :hint nil)
  "
Current thing: %s(current-forward-thing)
[_p_/_n_] line      [_u_/_v_] scroll   [_b_/_f_] thing
[_a_/_e_] begin/end [_m_/_x_] mark     [_SPC_] scroll
[_s_] symbol      [_c_] char         [_e_] list
[_d_] defun       [_w_] word
[_l_] center      [_q_] quit
"
  ("q" nil :exit t)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-thing*)
  ("b" backward-thing*)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("s" (setq current-forward-thing 'symbol))
  ("c" (setq current-forward-thing 'char))
  ("w" (setq current-forward-thing 'word))
  ("d" (setq current-forward-thing 'defun))
  ("e" (setq current-forward-thing 'list))
  ("v" scroll-up-command)
  ("SPC" scroll-up-command)
  ("u" scroll-down-command)
  ("m" set-mark-command)
  ("x" exchange-point-and-mark)
  ("l" recenter-top-bottom))

(defmacro core//define-use-other-window (-fn &optional -negative? -name)
  (unless -name (setq -name -fn))
  `(defun ,(intern (format "%s-other-window" -name))
       (&optional -n)
     (interactive "p")
     (when (= -n 0) (setq -n 1))
     ,(when -negative? '(setq -n (- -n)))
     (with-selected-window (next-window)
       (,-fn -n))))

(core//define-use-other-window forward-line)
(core//define-use-other-window forward-line :negative backward-line)
(core//define-use-other-window scroll-up-line)
(core//define-use-other-window scroll-down-line)

(defhydra hydra-move-other-window (:color pink :hint nil)
  "
[_u_/_v_] [_SPC_] [_p_/_n_] [_l_] [_DEL_/_RET_] [_q_]
"
  ("q" nil :exit t)
  ("n" forward-line-other-window)
  ("p" backward-line-other-window)
  ("DEL" scroll-down-line-other-window)
  ("RET" scroll-up-line-other-window)
  ("l" (with-selected-window (next-window)
         (recenter-top-bottom)))
  ("v" scroll-other-window)
  ("SPC" scroll-other-window)
  ("u" scroll-other-window-down))

(defun hydra-move-invoker (&optional -arg)
  (interactive "P")
  (if -arg
      (hydra-move-other-window/body)
    (hydra-move/body)))

(defhydra hydra-next-error (:color pink :hint nil)
  "
Compilation errors:
_n_: next error        _<_: first error
_p_: previous error    _`_: next error
_q_uit
"
  ("`" next-error)
  ("n" next-error)
  ("p" previous-error)
  ("<" first-error)
  ("q" nil :color blue))
