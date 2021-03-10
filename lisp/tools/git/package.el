;;; -*- lexical-binding: t; -*-

(require-packages!
 diff-hl
 gitignore-mode
 git-timemachine
 magit)

(autoload 'ymacs-hydra/smerge/body (expand! "commands") nil t)

(define-key! ("C-x g"))
(define-key! :prefix "C-x g"
  ("d" . magit-dispatch)
  ("b" . magit-blame)
  ("f" . magit-file-dispatch)
  ("=" . diff-hl-diff-goto-hunk)
  ("SPC" . diff-hl-mark-hunk)
  ("t" . ymacs-git/timemachine)
  ("n" . diff-hl-next-hunk)
  ("p" . diff-hl-previous-hunk)
  ("r" . diff-hl-revert-hunk)
  ("j" . ymacs-git/goto-hunk)
  ("g" . ymacs-git/status)
  ("c" . magit-checkout))
