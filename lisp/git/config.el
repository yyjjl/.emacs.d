;;; -*- lexical-binding: t; -*-

(after! diff-hl
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)

  ;; Highlight on-the-fly, too slow
  ;; (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)

  (setq diff-hl-margin-symbols-alist
        '((insert . "+")
          (delete . "-")
          (change . "=")
          (unknown . "?")
          (ignored . "."))))

(after! magit
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-auto-revert-mode nil))

(after! transient
  (setq transient-mode-line-format nil))

(after! magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . magit-status)
    ("C-x g")))
