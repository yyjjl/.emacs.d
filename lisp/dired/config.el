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
    (setq dired-listing-switches "-alh --group-directories-first")

    (define-key! :map dired-mode-map
      ("s" . hydra-dired-quick-sort/body)))

  (setq wdired-allow-to-change-permissions t)

  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
