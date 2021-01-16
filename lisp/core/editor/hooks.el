;; -*- lexical-binding: t -*-

(declare-function exec-path-from-shell-initialize "ext:exec-path-from-shell")

(define-advice indent-for-tab-command (:around (-fn &rest -arg) smart)
  (if (save-excursion
        (forward-line 0)
        (and outline-minor-mode (looking-at-p outline-regexp)))
      ;; Toggle outline
      (outline-toggle-children)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (do-complete (equal current-prefix-arg '(16))))
      (unless do-complete
        (apply -fn -arg))
      (when (or do-complete
                (and (equal old-point (point))
                     (equal old-tick (buffer-chars-modified-tick))
                     (called-interactively-p 'interactive)
                     (not (eq tab-always-indent 'complete))))
        ;; Trigger completions
        (or (ignore-errors (ymacs-editor//try-expand-local-snippets))
            (ignore-errors (call-interactively #'company-files))
            (ignore-errors (call-interactively #'company-complete))
            (ignore-errors (call-interactively #'hippie-expand)))))))

(define-advice compilation-start (:around (-fn &rest -args) set-env)
  (let* ((env (ymacs-editor//get-environment))
         (buffer (with-temp-env! env
                   (apply -fn -args))))
    (with-current-buffer buffer
      (when (or env compilation-environment)
        (save-excursion
          (goto-char (point-min))
          (when (let ((case-fold-search nil))
                  (search-forward mode-name nil t))
            (forward-line 1)
            (let ((inhibit-read-only t))
              (insert "\nEnvironments:\n  ")
              (insert (string-join (append env compilation-environment) "\n  "))
              (insert "\n"))))))
    buffer))

(defun ymacs-editor//after-init (&optional -frame)
  (when (daemonp)
    (remove-hook 'after-make-frame-functions #'ymacs-editor//after-init))

  (require 'lv)

  (ivy-mode 1)
  (counsel-mode 1)
  (semantic-mode 1)
  (projectile-mode 1)

  (global-company-mode 1)
  (yas-global-mode 1)

  (avy-setup-default)

  (when sys/macp
    (exec-path-from-shell-initialize))

  (when (display-graphic-p -frame)
    (when ymacs-fcitx-path
      (fcitx-aggressive-setup))

    (when ymacs-editor-use-childframe
      (company-posframe-mode 1)))

  (find-file-noselect (expand-cache! "org/*note*"))
  (find-file-noselect (expand-cache! "org/*task*"))

  (message "Init Time: %.3f (with %d packages activated)"
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list)))

(add-hook (if (daemonp)
              'after-make-frame-functions
            'after-init-hook)
          #'ymacs-editor//after-init)

(after! semantic
  (advice-add #'semantic-analyze-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-notc-completion-at-point-function :override #'ignore)
  (advice-add #'semantic-analyze-nolongprefix-completion-at-point-function :override #'ignore)

  (define-hook! ymacs-semantic//inhibit-function (semantic-inhibit-functions)
    (or (and default-directory (file-remote-p default-directory))
        (not (derived-mode-p 'prog-mode))))

  (define-advice semantic-idle-scheduler-function (:around (-fn &rest -args) allow-quit)
    (with-local-quit (apply -fn -args))))

(after! dired
  (define-hook! ymacs-editor//dired-setup (dired-mode-hook)
    (setq mode-line-buffer-identification '("%b" (dired-omit-mode " (omit)")))
    (dired-hide-details-mode 1)))

(after! ibuffer
  (define-hook! ymacs-ibuffer//setup (ibuffer-mode-hook)
    ;; (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")

    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))

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
        t)))

  (define-advice flycheck-add-overlay (:around (-fn &rest -args) set-priority)
    (let ((ov (apply -fn -args)))
      (when-let ((err (overlay-get ov 'flycheck-error))
                 (severity (-> err
                               flycheck-error-level
                               flycheck-error-level-severity)))
        (overlay-put ov 'priority (- severity 10)))
      ov)))

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-editor//hs-auto-expand)
  (advice-add #'xref-find-definitions :after #'ymacs-editor//hs-auto-expand))

(after! yasnippet
  (define-advice yas-next-field-or-maybe-expand (:around (-fn &rest -args) expand-local)
    (or (ymacs-editor//try-expand-local-snippets)
        (apply -fn -args)))

  (after! org
    (advice-add
     #'org-cycle
     :around #'yas-next-field-or-maybe-expand@expand-local)))

(after! graphviz-dot-mode
  (define-hook! ymacs-editor//dot-setup (graphviz-dot-mode-hook)
    (ymacs-editor//add-company-backend 'company-graphviz-dot-backend)))

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1))))
