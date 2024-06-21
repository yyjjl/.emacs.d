;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rect)
  (require 'display-line-numbers))

(require 'transient)

(transient-define-prefix ymacs-transient/sort ()
  [["Sort"
    ("r" "regexp" sort-regexp-fields)
    ("f" "fields" sort-fields)
    ("c" "columns" sort-columns)
    ("l" "lines" sort-lines)
    ("n" "numeric" sort-numeric-fields)]
   ["Flush/Keep"
    ("F" "flush" flush-lines)
    ("k" "keep" keep-lines)]])

(transient-define-prefix ymacs-transient/window ()
  ["Window Management"
   ["Resize"
    ("{" "->| |<-" shrink-window-horizontally :transient t)
    ("}" "<-| |->" enlarge-window-horizontally  :transient t)
    ("^" "enlarge" enlarge-window  :transient t)
    ("v" "shrink" shrink-window  :transient t)
    ("n" "balance" balance-windows  :transient t)]
   ["Split"
    ("H" "horizontally" ymacs-editor/window-split-horizontally  :transient t)
    ("V" "vertically" ymacs-editor/window-split-vertically  :transient t)
    ("|" "force horizontally " ymacs-editor/window-force-split-horizontally :transient t)
    ("_" "force vertically " ymacs-editor/window-force-split-vertically :transient t)]
   ["Zoom"
    ("+" "in" text-scale-increase :transient t)
    ("=" "in" text-scale-increase :transient t)
    ("-" "out" text-scale-decrease :transient t)
    ("0" "reset" (lambda () (interactive) (text-scale-increase 0)) :transient t)]])


(transient-define-prefix ymacs-transient/outline ()
  ["Outline"
   ["Hide"
    ("q" "Hide everything but the top-level headings" outline-hide-sublevels :transient t)
    ("t" "Hide everything but headings (all body lines)" outline-hide-body :transient t)
    ("o" "Hide other branches" outline-hide-other :transient t)
    ("c" "Hide this entry's body" outline-hide-entry :transient t)
    ("l" "Hide body lines in this entry and sub-entries" outline-hide-leaves :transient t)
    ("d" "Hide everything in this entry and sub-entries" outline-hide-subtree :transient t)
    ("TAB" "Toggle hide/show children" outline-toggle-children :transient t)]
   ["Show"
    ("a" "Show (expand) everything" outline-show-all :transient t)
    ("e" "Show this heading's body" outline-show-entry :transient t)
    ("i" "Show this heading's immediate child sub-headings" outline-show-children :transient t)
    ("k" "Show all sub-headings under this heading" outline-show-branches :transient t)
    ("s" "Show (expand) everything in this heading & below" outline-show-subtree :transient t)]
   ["Move"
    ("u" "Up" outline-up-heading :transient t)
    ("n" "Next" outline-next-visible-heading :transient t)
    ("p" "Prev" outline-previous-visible-heading :transient t)
    ("f" "Forward (same level)" outline-forward-same-level :transient t)
    ("b" "Backward (same level)" outline-backward-same-level :transient t)]])

(transient-define-prefix ymacs-transient/ediff ()
  ["Ediff"
   ["Buffers"
    ("b" "ediff-buffers" ediff-buffers)
    ("B" "ediff-buffers3" ediff-buffers3)]
   ["Files"
    ("=" "ediff-files" ediff-files)
    ("f" "ediff-files" ediff-files)
    ("F" "ediff-files3" ediff-files3)
    ("c" "ediff-current-file" ediff-current-file)]
   ["VC"
    ("r" "ediff-revision" ediff-revision)]
   ["Regions"
    ("l" "ediff-regions-linewise" ediff-regions-linewise)
    ("w" "ediff-regions-wordwise" ediff-regions-wordwise)]])

(transient-define-prefix ymacs-transient/query-replace ()
  [("r" "regexp" query-replace-regexp)
   ("RET" "text" query-replace)
   ("b" "re-builder" ymacs-editor/query-replace-regexp)
   ("SPC" "re-builder" ymacs-editor/query-replace-regexp)])
