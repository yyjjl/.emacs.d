;; -*- lexical-binding: t -*-

(load (expand! "hooks-builtin"))

(after! projectile
  (add-hook 'kill-emacs-hook #'projectile--cleanup-known-projects)
  (advice-add #'delete-file-projectile-remove-from-cache :around #'ignore-remote!)

  (define-advice projectile-invalidate-cache (:after (_) empty-vars)
    (dolist (buffer (ignore-errors (projectile-project-buffers)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (var ymacs-editor-projectile-invalidate-cache-empty-vars)
            (set var nil))))))

  (define-advice projectile-maybe-read-command (:around (-fn &rest -args) show-project-root)
    (lv-message "Current directory: %s" default-directory)
    (unwind-protect
        (apply -fn -args)
      (lv-delete-window)))

  (define-advice projectile-run-compilation (:override (-cmd) use-comint)
    (if (functionp -cmd)
        (funcall -cmd)
      (compile -cmd t)))

  (define-advice projectile-project-root (:override (&optional -dir) fast)
    (let ((dir (or -dir default-directory)))
      (if (file-remote-p dir)
          (projectile-root-local dir)
        (cl-subst
         nil 'none
         (or (cl-some
              (lambda (func)
                (let* ((cache-key (format "%s-%s" func dir))
                       (cache-value (gethash cache-key projectile-project-root-cache)))
                  (if (and cache-value (file-exists-p cache-value))
                      cache-value
                    (let ((value (funcall func (file-truename dir))))
                      (puthash cache-key value projectile-project-root-cache)
                      value))))
              projectile-project-root-functions)
             'none))))))

(after! ivy
  (add-hook 'ivy-occur-mode-hook #'ymacs-default//truncate-line)
  (add-hook 'ivy-occur-grep-mode-hook #'ymacs-default//truncate-line)
  (advice-add #'ivy--cleanup :before (lambda (&rest _) (lv-delete-window)))

  (define-advice ivy-occur-next-error (:around (-fn &rest -args) ensure-visible)
    (if-let (window (or (get-buffer-window (current-buffer))
                        (display-buffer (current-buffer))))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args)))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-help)
    (ymacs-editor//display-help -cmd)))

(after! company-capf
  (advice-add 'company-capf :around #'ignore-errors!))

(after! flycheck
  (define-advice flycheck-error-level-interesting-p (:override (err) smart)
    (when (flycheck-error-p err)
      (if-let ((min-severity (flycheck-error-level-severity flycheck-navigation-minimum-level)))
          (or (<= min-severity
                  (-> err
                      flycheck-error-level
                      flycheck-error-level-severity))
              ;; all errors have a severity smaller than min-severity
              (--all?
               (< (-> it
                      flycheck-error-level
                      flycheck-error-level-severity)
                  min-severity)
               flycheck-current-errors))
        t))))

(after! yasnippet
  (define-advice yas-next-field-or-maybe-expand (:around (-fn &rest -args) expand-local)
    (or (ymacs-editor//try-expand-local-snippets)
        (apply -fn -args)))

  (after! org
    (advice-add
     #'org-cycle
     :around #'yas-next-field-or-maybe-expand@expand-local)))

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1))))

(after! ace-window
  (define-advice aw-update (:override () ignore-on)
    (let ((aw-ignore-on t)
          (aw-ignore-current))
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize (apply #'string (reverse path)) 'face 'aw-mode-line-face)))))))
