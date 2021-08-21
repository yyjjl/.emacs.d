;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'smerge-mode))

(pretty-hydra-define ymacs-hydra/smerge
  (:title "Smerge" :color pink :quit-key "q")
  ("Move"
   (("n" smerge-next "next hunk")
    ("p" smerge-prev "prev hunk"))
   "Keep"
   (("b" smerge-keep-base "keep base")
    ("u" smerge-keep-upper "keep upper")
    ("l" smerge-keep-lower "keep lower")
    ("a" smerge-keep-all "keep all")
    ("RET" smerge-keep-current "keep current")
    ("C-m" smerge-keep-current "keep current"))
   "Diff"
   (("<" smerge-diff-base-upper "base/upper")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "refine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill")
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :exit t))))
