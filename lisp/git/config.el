;;; -*- lexical-binding: t; -*-

(after! git-gutter
  (when (fboundp 'define-fringe-bitmap)
    (require 'git-gutter-fringe))

  (when (fboundp 'define-fringe-bitmap)
    (set-face-foreground 'git-gutter-fr:modified "yellow")
    (set-face-foreground 'git-gutter-fr:added "green")
    (set-face-foreground 'git-gutter-fr:deleted "red")
    (setq git-gutter-fr:side 'left-fringe))
  (setq git-gutter:update-hooks '(after-save-hook))
  (setq git-gutter:handled-backends '(svn hg git)))

(after! magit
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-auto-revert-mode nil))

(after! transient
  (setq transient-mode-line-format nil))

(after! magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . magit-status)
    ("C-x g")))
