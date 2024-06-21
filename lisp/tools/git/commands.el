;;; -*- lexical-binding: t; -*-

(require 'smerge-mode)
(require 'transient)

(transient-define-prefix ymacs-transient/smerge ()
  [["Move"
    ("n" "next hunk" smerge-next :transient t)
    ("p" "prev hunk" smerge-prev :transient t)]
   ["Keep"
    ("b" "keep base" smerge-keep-base :transient t)
    ("u" "keep upper" smerge-keep-upper :transient t)
    ("l" "keep lower" smerge-keep-lower :transient t)
    ("a" "keep all" smerge-keep-all :transient t)
    ("RET" "keep current" smerge-keep-current :transient t)
    ("C-m" "keep current" smerge-keep-current :transient t)]
   ["Diff"
    ("<" "base/upper" smerge-diff-base-upper :transient t)
    ("=" "upper/lower" smerge-diff-upper-lower :transient t)
    (">" "base/lower" smerge-diff-base-lower :transient t)
    ("R" "refine" smerge-refine :transient t)
    ("E" "ediff" smerge-ediff :transient t)]
   ["Other"
    ("C" "combine" smerge-combine-with-next)
    ("r" "resolve" smerge-resolve)
    ("k" "kill" smerge-kill-current)
    ("ZZ" "Save and bury buffer"
     (lambda ()
       (interactive)
       (save-buffer)
       (bury-buffer)))]])
