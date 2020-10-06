;;; -*- lexical-binding: t; -*-

(after! diff-hl
  (setq diff-hl-draw-borders nil)

  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (after! magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(after! magit
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-auto-revert-mode nil))

(after! transient
  (setq transient-mode-line-format nil))

(after! magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . magit-status)
    ("C-x g")))
