;;; -*- lexical-binding: t; -*-

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

  (avy-setup-default)
  (ace-pinyin-global-mode 1)

  (persistent-scratch-autosave-mode 1)

  (ymacs-x//enable)

  (when sys/macp
    (exec-path-from-shell-initialize))

  (when (display-graphic-p -frame)
    ;; (mouse-avoidance-mode t)

    (fcitx-aggressive-setup)

    (when ymacs-editor-use-childframe-p
      (company-posframe-mode 1)))

  (find-file-noselect (expand-cache! "org/*note*"))

  (message "Init Time: %.3f (with %d packages activated)"
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list)))

(add-hook (if (daemonp)
              'after-make-frame-functions
            'after-init-hook)
          #'ymacs-editor//after-init
          100)

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

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-editor//hs-auto-expand)
  (advice-add #'xref-find-definitions :after #'ymacs-editor//hs-auto-expand))

(after! flymake
  (define-hook! ymacs-editor//setup-flymake (flymake-mode-hook)
    (when flymake-mode
      (unless next-error-function
        (setq-local next-error-function #'flymake-goto-next-error))))

  (define-advice flymake-goto-next-error (:around (-fn &optional -n -filter -interactive))
    (let ((min-severity (flymake--severity :warning)))
      (when (cl-some
             (lambda (ov)
               (when-let (diag (overlay-get ov 'flymake-diagnostic))
                 (>= (flymake--severity (flymake--diag-type diag)) min-severity)))
             (flymake--overlays))
        (setq -filter '(:error :warning)))
      (funcall -fn -n -filter -interactive))))
