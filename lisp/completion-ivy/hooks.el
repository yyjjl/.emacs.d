;; -*- lexical-binding: t -*-

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)

(after! ivy
  (define-hook! ymacs-ivy|occur-setup (ivy-occur-mode-hook ivy-occur-grep-mode-hook)
    (local-set-key "/" #'ymacs-ivy/occur-filter-lines)
    (local-set-key (kbd "C-/") #'ymacs-ivy/occur-undo)

    (setq truncate-lines t))

  (define-advice ivy-occur-next-error (:around (-fn &rest -args) ensure-visible)
    (if-let (window (or (get-buffer-window (current-buffer))
                        (display-buffer (current-buffer))))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args)))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-help)
    (ymacs-ivy//display-help -cmd))

  (define-advice counsel--grep-unwind (:after (&optional _) delete-lv-window)
    (lv-delete-window)))
