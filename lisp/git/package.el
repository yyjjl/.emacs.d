;;; -*- lexical-binding: t; -*-

(require-packages!
 diff-hl
 gitignore-mode
 gitconfig-mode
 git-timemachine
 git-messenger
 git-link
 magit)

(defvar ymacs-git--diff-hl-update-timer nil)
(defvar ymacs-git--diff-hl-update-delay 0.5)

(define-key! :prefix "C-x g"
  ("=" . diff-hl-diff-goto-hunk)
  ("SPC" . diff-hl-mark-hunk)
  ("t" . ymacs-git/timemachine)
  ("n" . diff-hl-next-hunk)
  ("p" . diff-hl-previous-hunk)
  ("r" . diff-hl-revert-hunk)
  ("j" . ymacs-git/goto-hunk)
  ("g" . magit-status)
  ("l" . git-link)
  ("c" . git-link-commit)
  ("m" . git-messenger:popup-message)
  ("b" . magit-checkout))
