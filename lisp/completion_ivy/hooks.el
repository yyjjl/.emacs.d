;; -*- lexical-binding: t -*-

(define-hook! :anonymous (kill-emacs-hook)
  (ignore-errors (ymacs//save-variable 'ivy-views ymacs-ivy-views-persistent-file)))

(after! ivy
  (define-hook! :anonymous (ivy-occur-mode-hook ivy-occur-grep-mode-hook)
    (local-set-key "/" #'ymacs-ivy/occur-filter-lines)
    (local-set-key (kbd "C-/") #'ymacs-ivy/occur-undo)

    (toggle-truncate-lines 1))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-command)
    (unless (stringp -cmd)
      (setq -cmd (string-join -cmd " ")))
    (lv-message (substitute-command-keys "Command(`\\[counsel-cd]' to cd): (@%s) %s")
                (propertize default-directory 'face font-lock-constant-face)
                (propertize -cmd 'face font-lock-doc-face)))

  (define-advice counsel--grep-unwind (:after (&optional _) delete-lv-window)
    (lv-delete-window)))
