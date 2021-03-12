;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rect)
  (require 'hydra)
  (require 'display-line-numbers))

(require 'pretty-hydra)

(defvar ymacs-hydra/global-toggles/keymap)
(declare-function ymacs-hydra/global-toggles/body 'hydra)
(declare-function ymacs-hydra/global-toggles/nil 'hydra)

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
      (define-key ymacs-hydra/global-toggles/keymap key #'ymacs-hydra/global-toggles/nil))

    (ymacs-hydra/global-toggles/body)))

(pretty-hydra-define ymacs-hydra/sort (:color red :quit-key ("q" "RET"))
  ("Sort"
   (("r" sort-regexp-fields "regexp")
    ("f" sort-fields "fields")
    ("c" sort-columns "columns")
    ("l" sort-lines "lines")
    ("n" sort-numeric-fields "numeric"))
   "Flush/Keep"
   (("F" flush-lines "flush")
    ("k" keep-lines "keep"))))

(pretty-hydra-define ymacs-hydra/window
  (:title "Window Management" :foreign-keys warn :quit-key "q")
  ("Resize"
   (("{" shrink-window-horizontally "->| |<-")
    ("}" enlarge-window-horizontally "<-| |->")
    ("^" enlarge-window "enlarge")
    ("v" shrink-window "shrink")
    ("n" balance-windows "balance"))
   "Split"
   (("H" ymacs-editor/window-split-horizontally "horizontally")
    ("V" ymacs-editor/window-split-vertically "vertically")
    ("|" ymacs-editor/window-force-split-horizontally "force horizontally ")
    ("_" ymacs-editor/window-force-split-vertically "force vertically "))
   "Zoom"
   (("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-increase 0) "reset"))
   "Appearance"
   (("F" set-frame-font "font")
    ("T" load-theme "theme"))))

(pretty-hydra-define ymacs-hydra/rectangle
  (
   :body-pre (rectangle-mark-mode 1)
   :color pink
   :post (deactivate-mark)
   :quit-key "q")
  ("Move"
   (("b" backward-char "←")
    ("f" forward-char "→")
    ("p" previous-line "↑")
    ("n" next-line "↓")
    ("a" beginning-of-line "Line begin")
    ("e" end-of-line "Line end"))
   "Action"
   (("w" copy-rectangle-as-kill "Copy")
    ("d" delete-rectangle "Delete")
    ("y" yank-rectangle "Yank")
    ("k" kill-rectangle "Kill")
    ("c" clear-rectangle "Replace with Spaces")
    ("s" string-rectangle "Replace with String")
    ("o" open-rectangle "Shift Right"))
   "Misc"
   (("m" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1))
     "toggle")
    ("x" rectangle-exchange-point-and-mark "Exchange")
    ("i" iedit-rectangle-mode "Iedit")
    ("N" rectangle-number-lines "Number")
    ("/" undo "Undo"))))

(pretty-hydra-define ymacs-hydra/outline
  (:title "Outline [`z' to quit]" :color amaranth :quit-key "z")
  ("Hide"
   (("q" outline-hide-sublevels) ; Hide everything but the top-level headings
    ("t" outline-hide-body) ; Hide everything but headings (all body lines)
    ("o" outline-hide-other)            ; Hide other branches
    ("c" outline-hide-entry)            ; Hide this entry's body
    ("l" outline-hide-leaves) ; Hide body lines in this entry and sub-entries
    ("d" outline-hide-subtree) ; Hide everything in this entry and sub-entries
    ("TAB" outline-toggle-children))
   "Show"
   (("a" outline-show-all)              ; Show (expand) everything
    ("e" outline-show-entry)            ; Show this heading's body
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

(defhydra ymacs-hydra/mc (:color red :hint nil)
  ""
  ;; ("." mc/mark-next-like-this "next")
  ("=" mc/mark-next-like-this "next")
  ;; ("," mc/mark-previous-like-this "prev")
  ("-" mc/mark-previous-like-this "prev")
  ("<" mc/skip-to-previous-like-this "skip prev")
  (">" mc/skip-to-next-like-this "skip next")
  ("RET" nil)
  ("q" nil))
