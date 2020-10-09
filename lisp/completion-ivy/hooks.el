;; -*- lexical-binding: t -*-

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)

(after! ivy
  (define-hook! ymacs-ivy|occur-setup (ivy-occur-mode-hook ivy-occur-grep-mode-hook)
    (local-set-key "/" #'ymacs-ivy/occur-filter-lines)
    (local-set-key (kbd "C-/") #'ymacs-ivy/occur-undo)

    (setq truncate-lines t))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-command)
    (unless (stringp -cmd)
      (setq -cmd (string-join -cmd " ")))
    (let ((help-string
           (apply
            #'concat
            (--map
             (when-let (keys (where-is-internal (car it)))
               (format "[%s to %s] "
                       (mapconcat
                        (lambda (key)
                          (propertize (key-description key) 'face 'hl-line))
                        keys
                        ", ")
                       (cdr it)))
             ymacs-ivy-grep-help-commands))))
      (unless (string-empty-p help-string)
        (setq help-string (concat help-string "\n")))
      (lv-message
       (format "%s(@%s) %s"
               help-string
               (propertize default-directory 'face font-lock-constant-face)
               (propertize -cmd 'face font-lock-doc-face)))))

  (define-advice counsel--grep-unwind (:after (&optional _) delete-lv-window)
    (lv-delete-window)))
