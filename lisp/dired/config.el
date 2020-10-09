;;; -*- lexical-binding: t; -*-

(after! dired
  (define-key! :map dired-mode-map
    (")" . dired-omit-mode)
    ("E" . ymacs-dired/open-externally)
    ("M-p" . dired-prev-subdir)
    ("M-n" . dired-next-subdir)
    (";" . dired-kill-subdir))

  (require 'dired-x)

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-alh")
  (setq wdired-allow-to-change-permissions t)

  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
