;;; -*- lexical-binding: t; -*-

(after! vc-hooks
  (setq vc-handled-backends '(Git)))

(setq diff-hl-command-prefix (kbd "C-x \\"))
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

  (define-key! :map magit-blob-mode-map
    ("C-x C-f" . magit-find-file))

  (define-key! :map magit-section-mode-map
    (("M-1" "M-2" "M-3" "M-4"))
    ("` 1" . magit-section-show-level-1-all)
    ("` 2" . magit-section-show-level-2-all)
    ("` 3" . magit-section-show-level-3-all)
    ("` 4" . magit-section-show-level-4-all))

  (setq magit-diff-refine-hunk t)
  (setq magit-auto-revert-mode nil))

(after! magit-blame
  (add-hook 'magit-blame-mode-hook #'read-only-mode)

  (setq magit-blame-echo-style 'margin)

  (add-to-list
   'mode-line-misc-info
   '(magit-blame-mode
     (""
      (:propertize "Blame" face font-lock-builtin-face)
      (magit-blame-read-only-mode "%%")
      " "))))

(after! transient
  (define-key! :map transient-map
    ("M-n" . transient-history-next)
    ("M-p" . transient-history-prev))
  (setq transient-mode-line-format nil))

;; Resolve diff3 conflicts
(after! smerge-mode
  (define-key! :map smerge-mode-map
    ("C-c M" . ymacs-transient/smerge)))
