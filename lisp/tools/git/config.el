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
  (define-key! :map magit-mode-map
    ("C-x C-f" . magit-find-file))

  (define-key! :map magit-section-mode-map
    ("M-1")
    ("M-2")
    ("M-3")
    ("M-4")
    ("` 1" . magit-section-show-level-1-all)
    ("` 2" . magit-section-show-level-2-all)
    ("` 3" . magit-section-show-level-3-all)
    ("` 4" . magit-section-show-level-4-all))

  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-auto-revert-mode nil))

(after! transient
  (define-key! :map transient-map
    ("M-n" . transient-history-next)
    ("M-p" . transient-history-prev))
  (setq transient-mode-line-format nil))

;; Resolve diff3 conflicts
(after! smerge-mode
  (define-key! :map smerge-mode-map
    ("C-c M" . ymacs-hydra/smerge/body)))
