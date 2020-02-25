;;; -*- lexical-binding: t; -*-

(require-packages! dired-narrow)

(config! dired
  :bind
  (:map dired-mode-map
   (")" . dired-omit-mode)
   ("E" . dired/open-externally)
   ("/" . dired-narrow-fuzzy)
   ("\\" . dired-narrow)
   ("M-p" . dired-prev-subdir)
   ("M-n" . dired-next-subdir)
   (";" . dired-kill-subdir))

  :hook
  (setup
   :define (dired-mode-hook)
   (setq mode-line-buffer-identification
         '("%b" (dired-omit-mode " (omit)")))
   ;; (auto-revert-mode 1)
   (dired-hide-details-mode 1))

  :config
  (require 'dired-x)

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-alh")
  (setq wdired-allow-to-change-permissions t)

  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))


(provide 'init-dired)
