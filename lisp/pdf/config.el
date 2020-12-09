;; -*- lexical-binding: t; -*-

(after! pdf-view
  (after! hippie-exp
    (add-to-list 'hippie-expand-ignore-buffers 'pdf-view-mode)
    (add-to-list 'hippie-expand-ignore-buffers 'doc-view-mode))

  (define-key! :map pdf-view-mode-map
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward))

  (setq pdf-annot-activate-created-annotations t)

  (after! pdf-annot
    (define-key! :map pdf-annot-edit-contents-minor-mode-map
      ("C-C C-k" . pdf-annot-edit-contents-abort)
      ("C-x C-s" .
       (lambda!
         (message
          (substitute-command-keys
           "Please use \\[pdf-annot-edit-contents-commit] to save content")))))

    (setq pdf-annot-edit-contents-setup-function
          (lambda (_) (org-mode)))))

(after! org
  (setcdr (assoc 'file org-link-frame-setup) #'ymacs-pdf//open-file)

  (org-pdftools-setup-link))

(after! org-noter
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-always-create-frame t)

  (dolist (map (list org-noter-doc-mode-map org-noter-notes-mode-map))
    (define-key! :map map
      ("C-." . org-noter-sync-current-page-or-chapter)
      ("C-M-p" . org-noter-sync-prev-page-or-chapter)
      ("C-M-." . org-noter-sync-current-page-or-chapter)
      ("C-M-n" . org-noter-sync-next-page-or-chapter)
      ("M-p" . org-noter-sync-prev-note)
      ("M-." . org-noter-sync-current-note)
      ("M-n" . org-noter-sync-next-note))))