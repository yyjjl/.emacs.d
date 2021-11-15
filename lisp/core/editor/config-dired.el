;;; -*- lexical-binding: t; -*-

(after! dired
  (setq dired-kill-when-opening-new-dired-buffer t)

  (define-hook! ymacs-editor//dired-setup (dired-mode-hook)
    (setq mode-line-buffer-identification '("%b" (dired-omit-mode " (omit)")))
    (dired-hide-details-mode 1))

  (define-key! :map dired-mode-map
    ("\\" . dired-compare-directories)
    ("]" . dired-omit-mode)
    ("[" . dired-hide-details-mode)
    ("E" . ymacs-editor/find-file-externally)
    ("M-p" . dired-prev-subdir)
    ("M-n" . dired-next-subdir)
    (";" . dired-kill-subdir))

  (require 'dired-x)

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-switches-in-mode-line 'as-is)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when ymacs-gls-path
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (if (and sys/macp (not ymacs-gls-path))
      (setq dired-listing-switches "-alh")

    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  (setq wdired-allow-to-change-permissions t))
