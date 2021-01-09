;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rect)
  (require 'hydra)
  (require 'display-line-numbers))

(require 'pretty-hydra)

;;;###autoload
(defun ymacs-hydra/toggles ()
  (interactive)
  (let ((extra-keys (where-is-internal #'ymacs-hydra/toggles)))
    (eval
     `(pretty-hydra-define ymacs-hydra/global-toggles
        (:title "Hydra Menu" :color amaranth :quit-key "q")
        ,(cl-loop
          for (name . groups) in ymacs-editor-toggles-alist
          for rows = (cl-loop
                      for (condition . rows) in groups
                      when (eval condition)
                      append rows)
          when rows
          append (list name rows))))

    (dolist (key extra-keys)
      (define-key ymacs-hydra/global-toggles/keymap
        key
        #'ymacs-hydra/global-toggles/nil))

    (ymacs-hydra/global-toggles/body)))

(defhydra ymacs-hydra/sort (:color red)
  "Sort"
  ("r" sort-regexp-fields "regexp")
  ("f" sort-fields "fields")
  ("c" sort-columns "columns")
  ("l" sort-lines "lines")
  ("n" sort-numeric-fields "numeric")
  ("F" flush-lines "flush")
  ("k" keep-lines "keep")
  ("RET" nil "quit")
  ("q" nil "quit"))

(pretty-hydra-define ymacs-hydra/window
  (:title "Window Management" :foreign-keys warn :quit-key "q")
  ("Resize"
   (("{" shrink-window-horizontally "->| |<-")
    ("}" enlarge-window-horizontally "<-| |->")
    ("^" enlarge-window "enlarge")
    ("v" shrink-window "shrink")
    ("n" balance-windows "balance"))
   "Split"
   (("H" ymacs-window/split-horizontally "horizontally")
    ("V" ymacs-window/split-vertically "vertically")
    ("|" ymacs-window/force-split-horizontally "force horizontally ")
    ("_" ymacs-window/force-split-vertically "force vertically "))
   "Zoom"
   (("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-increase 0) "reset"))
   "Appearance"
   (("F" set-frame-font "font")
    ("T" load-theme "theme"))))

(defhydra ymacs-hydra/rectangle (:body-pre (rectangle-mark-mode 1)
                                 :color pink
                                 :post (deactivate-mark))
  "
  ^_p_^         [_k_]kill      [_s_]string
_b_   _f_       [_q_]quit      [_y_]yank
  ^_n_^         [_m_]mark      [_w_]copy
^^^^            [_x_]exchange  [_/_]undo     [_i_]iedit
^^^^            [_a_]line beg  [_e_]line end [_N_]number
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
  ("i" iedit-rectangle-mode nil)
  ("y" yank-rectangle nil)
  ("/" undo nil)
  ("N" rectangle-number-lines nil :exit t)
  ("s" string-rectangle nil)
  ("k" kill-rectangle nil)
  ("q" nil nil))

(pretty-hydra-define ymacs-hydra/outline
  (:title "Outline [`z' to quit]" :color amaranth :quit-key "z")
  ("Hide"
   (("q" outline-hide-sublevels)   ; Hide everything but the top-level headings
    ("t" outline-hide-body)     ; Hide everything but headings (all body lines)
    ("o" outline-hide-other)    ; Hide other branches
    ("c" outline-hide-entry)    ; Hide this entry's body
    ("l" outline-hide-leaves)   ; Hide body lines in this entry and sub-entries
    ("d" outline-hide-subtree)  ; Hide everything in this entry and sub-entries
    )
   "Show"
   (("a" outline-show-all)   ; Show (expand) everything
    ("e" outline-show-entry) ; Show this heading's body
    ("i" outline-show-children) ; Show this heading's immediate child sub-headings
    ("k" outline-show-branches) ; Show all sub-headings under this heading
    ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
    )
   "Move"
   (("u" outline-up-heading)            ; Up
    ("n" outline-next-visible-heading)  ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    )))

(pretty-hydra-define ymacs-hydra/ediff
  (:title "Ediff" :color blue)
  ("Buffers"
   (("b" ediff-buffers)
    ("B" ediff-buffers3))
   "Files"
   (("=" ediff-files)
    ("f" ediff-files)
    ("F" ediff-files3)
    ("c" ediff-current-file))
   "VC"
   (("r" ediff-revision))
   "Regions"
   (("l" ediff-regions-linewise)
    ("w" ediff-regions-wordwise))))

(defhydra ymacs-hydra/next-error (:color pink :hint nil)
  "Error"
  ("`" next-error "next")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("<" first-error "first")
  ("q" nil "quit" :exit t))

(defhydra ymacs-hydra/mc (:color blue :hint nil)
  ""
  ("." mc/mark-next-like-this "next" :exit nil)
  ("=" mc/mark-next-like-this "next" :exit nil)
  ("," mc/mark-previous-like-this "prev" :exit nil)
  ("-" mc/mark-previous-like-this "prev" :exit nil)
  ("<" mc/skip-to-previous-like-this "skip prev" :exit nil)
  (">" mc/skip-to-next-like-this "skip next" :exit nil)
  ("RET" nil)
  ("q" nil))

(defhydra ymacs-hydra/games (:color blue :hint nil)
  "
[_5_] 5x5            [_a_] animate      [_g_] gomoku    [_h_] hanoi   [_l_] life
[_m_] mpuz           [_p_] pong         [_t_] tetris    [_z_] zone    ^ ^
[_b 1_] blackbox     [_b 2_] bubbles    ^ ^             ^ ^           ^ ^
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
  ("s 1" snake)
  ("s 2" solitaire)
  ("t" tetris)
  ("z" zone))
