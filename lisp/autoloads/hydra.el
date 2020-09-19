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
   (("I"
     (setq doom-modeline-icon (not doom-modeline-icon))
     "display icons"
     :toggle doom-modeline-icon)
    ("V"
     (core-view-code-mode (if core-view-code-mode -1 1))
     "view code"
     :toggle core-view-code-mode)
    ("E"
     toggle-debug-on-error
     "debug on error"
     :toggle (default-value 'debug-on-error))
    ("Q"
     toggle-debug-on-quit "debug on quit"
     :toggle (default-value 'debug-on-quit))
    ("W"
     (setq show-trailing-whitespace (not show-trailing-whitespace))
     "trailing whitespace"
     :toggle show-trailing-whitespace)
    ("N"
     (display-line-numbers-mode (if display-line-numbers-mode -1 1))
     "line number"
     :toggle display-line-numbers-mode)
    ("B" display-battery-mode "battery" :toggle t)
    ("T" display-time-mode "time" :toggle t))))

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
    (where-is-internal 'hydra-global-toggles/body nil t)
    #'hydra-global-toggles/nil)
  (prog1 (eval hydra-global-toggles/hint)
    (setq hydra-global-toggles/hint hydra-global-toggles/hint-cache)))

;; add additional columns dynamicly
(setq hydra-global-toggles/hint-cache '(hydra-global-toggles/update))
(setq hydra-global-toggles/hint hydra-global-toggles/hint-cache)

(defhydra hydra-sort (:color red)
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

(defhydra hydra-resize-window (:color pink)
  "Window Size"
  ("{" shrink-window-horizontally "-><-")
  ("}" enlarge-window-horizontally "<-->")
  ("^" enlarge-window "enlarge")
  ("-" shrink-window "shrink")
  ("RET" nil "quit")
  ("q" nil "quit"))

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

(pretty-hydra-define hydra-outline
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

(defhydra hydra-next-error (:color pink :hint nil)
  "Error"
  ("`" next-error "next")
  ("n" next-error "next")
  ("p" previous-error "prev")
  ("<" first-error "first")
  ("q" nil "quit" :exit t))
