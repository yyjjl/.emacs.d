;;; -*- lexical-binding: t; -*-

(after! diff-hl
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'right)

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
  (define-key! :map magit-mode-map
    ("C-x C-f" . magit-find-file))

  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-auto-revert-mode nil))

(after! transient
  (setq transient-mode-line-format nil))

(after! magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . ymacs-git//status)
    ("C-x g")))

;; Resolve diff3 conflicts
(after! smerge-mode
  (define-key! :map smerge-mode-map
    ("C-c M" . ymacs-hydra/smerge/body)))
