;;; -*- lexical-binding: t; -*-

(require-packages!
 gitignore-mode
 gitconfig-mode
 git-gutter
 git-gutter-fringe
 git-timemachine
 git-messenger
 git-link
 magit)

(define-key! :prefix "C-x g"
  ("h" . git-gutter:popup-hunk)
  ("s" . git-gutter:stage-hunk)
  ("r" . git-gutter:revert-hunk)
  ("t" . ymacs-git/timemachine)
  ("n" . git-gutter:next-hunk)
  ("p" . git-gutter:previous-hunk)
  ("j" . ymacs-git/goto-gutter)
  ("g" . magit-status)
  ("l" . git-link)
  ("c" . git-link-commit)
  ("m" . git-messenger:popup-message)
  ("b" . magit-checkout))

(provide 'init-git)
