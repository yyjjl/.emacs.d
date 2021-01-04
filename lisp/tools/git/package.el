;;; -*- lexical-binding: t; -*-

(require-packages!
 diff-hl
 gitignore-mode
 gitconfig-mode
 git-timemachine
 magit)

(defvar ymacs-git--diff-hl-update-timer nil)
(defvar ymacs-git--diff-hl-update-delay 0.5)

(autoload 'ymacs-hydra/smerge/body (expand! "commands") nil t)

(define-key! ("C-x g"))
(define-key! :prefix "C-x g"
  ("G" . magit-dispatch)
  ("f" . magit-file-dispatch)
  ("=" . diff-hl-diff-goto-hunk)
  ("SPC" . diff-hl-mark-hunk)
  ("t" . ymacs-git/timemachine)
  ("n" . diff-hl-next-hunk)
  ("p" . diff-hl-previous-hunk)
  ("r" . diff-hl-revert-hunk)
  ("j" . ymacs-git/goto-hunk)
  ("g" . ymacs-git//status)
  ("b" . magit-checkout))

(define-key! :prefix "C-x j"
  ("j" . ymacs-git/goto-hunk))
