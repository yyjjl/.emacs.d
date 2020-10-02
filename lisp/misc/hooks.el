;; -*- lexical-binding: t -*-

(define-hook! ymacs-misc|after-init (after-init-hook)
  (recentf-mode 1)
  (winner-mode 1)

  (savehist-mode 1)
  (save-place-mode 1)

  (yas-global-mode 1)

  (which-key-mode 1)

  (projectile-mode 1)

  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)

  (global-whitespace-mode 1)
  (global-hl-todo-mode 1)
  (electric-indent-mode 1)

  (ignore-errors (global-so-long-mode 1)))

(define-hook! ymacs-misc|after-init-idle (ymacs-after-init-idle-hook)
  (when (and ymacs-fcitx-path (display-graphic-p))
    (fcitx-aggressive-setup))

  (find-file-noselect (expand-var! "org/*note*"))
  (find-file-noselect (expand-var! "org/*task*"))

  (desktop-save-mode 1))


(define-advice next-error (:around (-fn &rest -args) smart)
  (let ((occur-buffer (ymacs-misc//get-occur-buffer)))
    (if-let (window (and occur-buffer (get-buffer-window occur-buffer)))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args))))

(define-advice indent-for-tab-command (:around (-fn &rest -arg) smart)
  (if (save-excursion
        (forward-line 0)
        (and outline-minor-mode (looking-at-p outline-regexp)))
      ;; Toggle outline
      (outline-toggle-children)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (apply -fn -arg)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 (called-interactively-p 'interactive)
                 (not (eq tab-always-indent 'complete)))
        (cond ;; Skip close paren
         ((memq (char-after) ymacs-misc--indent-close-list)
          (forward-char 1))
         ;; Trigger completions
         ((and (not (eq tab-always-indent 'complete))
               (not (memq (get-text-property (max (point-min) (1- (point))) 'face)
                          '(font-lock-string-face font-lock-doc-face)))
               (looking-back "\\(?:\\s_\\|\\sw\\)\\(?:\\.\\|->\\|::?\\)?"
                             (max (point-min) (- (point) 5))))
          ;; If company-idle-delay is nil (which means company is not trigger
          ;; automatically, <tab> will trigger it
          (catch 'done
            (dolist (func ymacs-misc--indent-compelte-functions)
              (when (ignore-errors (call-interactively func))
                (throw 'done nil))))))))))

(after! savehist
  (define-advice savehist-save
      (:around (-fn &optional -auto-save) set-additional-variables)
    (let ((variables savehist-additional-variables))
      (dolist (symbol (apropos-internal "-\\(ring\\|history\\)\\'" 'boundp))
        (unless (or (memq symbol ymacs-savehist-exclude-variables)
                    (memq symbol savehist-minibuffer-history-variables)
                    (keywordp symbol))
          (cl-pushnew symbol variables)))
      (let ((savehist-additional-variables variables))
        (funcall -fn -auto-save)))))

(after! saveplace
  (add-hook 'ymacs-autosave-hook #'save-place-kill-emacs-hook))

(after! desktop
  (define-advice desktop-save (:around (-fn &rest -args) unless-loaded)
    (if (or (called-interactively-p 'interactive)
            desktop-file-modtime)
        (apply -fn -args)
      (message "Current desktop was not loaded from a file. Ignored")))

  (define-advice desktop-read (:around (-fn &rest -args) disable-semantic) ;
    "Temporarily disable semantic mode when load desktop"
    (let ((semantic-enable-p semantic-mode))
      (semantic-mode -1)
      (apply -fn -args)
      (when semantic-enable-p
        (semantic-mode 1)))))

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
  (advice-add #'ffap-guesser :around #'ignore-errors!))

(after! xref
  (define-hook! :anonymous (xref--xref-buffer-mode-hook)
    (toggle-truncate-lines 1))

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

  (define-advice projectile-edit-dir-locals (:override (&optional -directory) auto-save)
    "Edit or create a .dir-locals.el file of the project."
    (interactive
     (list (let ((root (projectile-project-root)))
             (or (and current-prefix-arg
                      (read-directory-name "Select root" root))
                 root))))
    (let ((default-directory -directory))
      (condition-case nil
          (call-interactively #'add-dir-local-variable)
        (quit
         (when-let ((files (dir-locals--all-files -directory)))
           (if (= (length files) 1)
               (find-file (car files))
             (find-file (ivy-read "Open file: " files :require-match t))))))))

  ;; Projectile root-searching functions can cause an infinite cl-loop on TRAMP
  ;; connections, so disable them.
  (advice-add #'projectile-locate-dominating-file :around #'ignore-remote!))

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
