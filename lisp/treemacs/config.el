;;; -*- lexical-binding: t; -*-

;; (after! winum
;;   (define-key! :map winum-keymap
;;     ("M-`" . ymacs-treemacs/switch)
;;     ("M-1" . ymacs-treemacs/select-window-1)))

(after! treemacs
  (define-key! :map treemacs-mode-map
    ("E" . treemacs-edit-workspaces))

  (treemacs-fringe-indicator-mode -1)
  (treemacs-tag-follow-mode 1)

  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 1.5
        treemacs-display-in-side-window t
        treemacs-eldoc-display t
        treemacs-file-event-delay 5000
        treemacs-file-follow-delay 0.5
        treemacs-follow-after-init t
        treemacs-git-command-pipe ""
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-indentation 1
        treemacs-indentation-string " "
        treemacs-is-never-other-window nil
        treemacs-max-git-entries 5000
        treemacs-no-png-images t
        treemacs-no-delete-other-windows t
        treemacs-project-follow-cleanup nil
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor t
        treemacs-show-hidden-files t
        treemacs-silent-filewatch nil
        treemacs-silent-refresh nil
        treemacs-sorting 'alphabetic-desc
        treemacs-space-between-root-nodes t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.5
        treemacs-width 30))
