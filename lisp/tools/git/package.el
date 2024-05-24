;;; -*- lexical-binding: t; -*-

(require-packages!
 diff-hl
 git-modes
 git-timemachine
 magit)

(autoload 'ymacs-hydra/smerge/body (expand! "commands") nil t)

(defalias 'magit-diff-two-branches #'magit-diff-range)

(define-key! ("C-x g"))
(define-key! :prefix "C-x g"
  ("d" . magit-dispatch)
  ("b" . magit-blame)
  ("f" . magit-file-dispatch)
  ("l" . magit-log-buffer-file)
  ("=" . diff-hl-diff-goto-hunk)
  ("SPC" . diff-hl-mark-hunk)
  ("t" . git-timemachine)
  ("n" . diff-hl-next-hunk)
  ("p" . diff-hl-previous-hunk)
  ("r" . diff-hl-revert-hunk)
  ("g" . magit-status)
  ("c" . magit-checkout))
