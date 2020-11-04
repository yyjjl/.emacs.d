;; -*- lexical-binding: t -*-

(define-hook! ymacs-misc|after-init (after-init-hook)
  (recentf-mode 1)
  (winner-mode 1)

  (savehist-mode 1)
  (save-place-mode 1)

  (yas-global-mode 1)

  (projectile-mode 1)

  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)

  (global-whitespace-mode 1)
  (global-hl-todo-mode 1)
  (electric-indent-mode 1)

  ;; (which-function-mode 1)
  (which-key-mode 1)

  (ignore-errors (global-so-long-mode 1)))

(define-hook! ymacs-misc|after-init-idle (ymacs-after-init-idle-hook)
  (when (and ymacs-fcitx-path (display-graphic-p))
    (fcitx-aggressive-setup))

  (find-file-noselect (expand-var! "org/*note*"))
  (find-file-noselect (expand-var! "org/*task*"))

  (message "Init Time: %.3f (with %d packages activated)"
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list)))

(define-advice indent-for-tab-command (:around (-fn &rest -arg) smart)
  (if (save-excursion
        (forward-line 0)
        (and outline-minor-mode (looking-at-p outline-regexp)))
      ;; Toggle outline
      (outline-toggle-children)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (apply -fn -arg)
      (when (and (equal old-point (point))
                 (equal old-tick (buffer-chars-modified-tick))
                 (called-interactively-p 'interactive)
                 (not (eq tab-always-indent 'complete)))
        ;; Trigger completions
        (or (ignore-errors (ymacs-misc//try-expand-local-snippets))
            (ignore-errors (call-interactively #'company-files))
            (when (looking-back "\\(?:\\s_\\|\\sw\\)\\(?:\\.\\|->\\|::?\\)?"
                                (max (point-min) (- (point) 5)))
              (ignore-errors (call-interactively #'company-complete)))
            (ignore-errors (call-interactively #'hippie-expand)))))))

(after! savehist
  (define-advice savehist-save
      (:around (-fn &optional -auto-save) set-additional-variables)
    (let ((variables savehist-additional-variables)
          (kill-ring (ymacs-misc//filter-ring kill-ring))
          (ivy-history (ignore-errors (ymacs-misc//filter-ring ivy-history))))
      (dolist (symbol (apropos-internal "-\\(ring\\|history\\)\\'" 'boundp))
        (unless (or (memq symbol ymacs-savehist-exclude-variables)
                    (memq symbol savehist-minibuffer-history-variables)
                    (keywordp symbol))
          (cl-pushnew symbol variables)))
      (let ((savehist-additional-variables variables))
        (funcall -fn -auto-save)))))

(after! saveplace
  (add-hook 'ymacs-autosave-hook #'save-place-kill-emacs-hook))

(after! recentf
  (add-hook 'ymacs-autosave-hook #'recentf-save-list))

(after! bookmark
  (define-hook! ymacs-misc|bookmark-setup (find-file-hook)
    (unless (file-remote-p default-directory)
      ;; Setup default bookmark
      (setq bookmark-current-bookmark
            (ignore-errors
              (cl-loop for (name . record) in bookmark-alist
                       when (equal (file-truename (buffer-file-name))
                                   (file-truename (bookmark-get-filename name)))
                       return name))))))

(after! ffap
  (advice-add #'ffap-guesser :around #'ignore-remote!))

(after! xref
  (define-hook! :anonymous (xref--xref-buffer-mode-hook)
    (setq truncate-lines t))

  (define-advice xref-find-definitions (:around (-fn -identifier) fallback-to-rg)
    (condition-case err
        (funcall -fn -identifier)
      (user-error
       (if-let* ((symbol (thing-at-point 'symbol)))
           (counsel-rg symbol)
         (message "%s" (error-message-string err)))))))

(after! flymake
  (define-hook! ymacs-misc|flymake-setup (flymake-mode-hook)
    (if flymake-mode
        (progn
          (put 'next-error-function
               'flymake-old-next-error-function
               next-error-function)
          (setq next-error-function 'flymake-goto-next-error))
      (setq next-error-function (get 'next-error-function
                                     'flymake-old-next-error-function)))))

(after! flycheck
  (define-advice flycheck-add-overlay (:around (-fn &rest -args) set-priority)
    (let ((ov (apply -fn -args)))
      (overlay-put ov 'priority -10)
      ov)))

(after! projectile
  (add-hook 'kill-emacs-hook #'projectile-cleanup-known-projects)

  (define-advice projectile-invalidate-cache (:after (_) empty-vars)
    (dolist (buffer (ignore-errors (projectile-project-buffers)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (var ymacs-misc-projectile-invalidate-cache-empty-vars)
            (set var nil))))))

  (define-advice projectile-run-compilation (:override (-cmd) show-compile-buffer)
    (if (functionp -cmd)
        (funcall -cmd)
      (compile -cmd t)))

  (define-advice projectile-maybe-read-command (:around (-fn &rest -args) show-project-root)
    (lv-message "Current directory: %s" default-directory)
    (unwind-protect
        (apply -fn -args)
      (lv-delete-window)))

  (define-advice projectile-project-root (:override (&optional -dir) light)
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
              projectile-project-root-files-functions)
             'none)))))

  (advice-add #'delete-file-projectile-remove-from-cache :around #'ignore-remote!))

(after! yasnippet
  (define-advice yas-next-field-or-maybe-expand (:around (-fn &rest -args) expand-local)
    (or (ymacs-misc//try-expand-local-snippets)
        (apply -fn -args)))

  (after! org
    (advice-add
     #'org-cycle
     :around #'yas-next-field-or-maybe-expand@expand-local)))

(after! graphviz-dot-mode
  (define-hook! ymacs-misc|dot-setup (graphviz-dot-mode-hook)
    (hs-minor-mode 1)
    (ymacs-company//add-backend 'company-graphviz-dot-backend)))
