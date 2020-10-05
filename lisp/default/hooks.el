;; -*- lexical-binding: t; -*-

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; Display long lines in truncated style (end line with $)
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines t)))
;; Turns on `auto-fill-mode', don't use `text-mode-hook'
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(define-hook! ymacs|create-missing-directories-h (find-file-not-found-functions)
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p (buffer-file-name))
    (let ((parent-directory (file-name-directory (buffer-file-name))))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))

(define-hook! ymacs|minibuffer-setup (minibuffer-setup-hook)
  (setq line-spacing 4)
  (setq gc-cons-threshold most-positive-fixnum))

(define-hook! ymacs|minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold ymacs-gc-cons-threshold))

(define-hook! (ymacs|default-frame-setup &optional frame)
  (window-setup-hook ;; when setup
   after-make-frame-functions)
  (set-frame-parameter frame 'buffer-predicate #'ymacs//buffer-predicate))

;; Make scratch buffer un-killable
(define-hook! ymacs|unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*task*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

;; ANSI-escape coloring in compilation-mode
(define-hook! ymacs|colorize-compilation-buffer (compilation-filter-hook)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(define-hook! (ymacs|compilation-finish-hook -buffer _)
  (compilation-finish-functions)
  (when (buffer-live-p -buffer)
    (with-current-buffer -buffer
      (unless (eq major-mode 'compilation-mode)
        ;; Sometime it will open a comint buffer
        (compilation-mode)
        ;; Record window as popup window
        (ymacs-popups//push-window (get-buffer-window -buffer) -buffer :autoclose)))))

;; Default prog-mode setup
(define-hook! ymacs|generic-prog-mode-setup (prog-mode-hook LaTeX-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (condition-case err
      (hs-minor-mode 1)
    (user-error (message "%s" (error-message-string err))))

  (hl-line-mode 1)
  (ignore-errors (hl-fill-column-mode 1))

  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

(define-hook! ymacs|generic-text-mode-setup (text-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (hl-line-mode 1)
  (ignore-errors (hl-fill-column-mode 1))

  (setq indicate-empty-lines t))

(define-hook! ymacs|generic-comint-mode-setup (comint-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (setq show-trailing-whitespace nil)
  (setq-local company-idle-delay nil))

(define-hook! ymacs|hack-local-variables (after-save-hook)
  (executable-make-buffer-file-executable-if-script-p)

  (when (and buffer-file-name
             (member (file-name-nondirectory buffer-file-name)
                     (eval-and-compile
                       (list dir-locals-file
                             (concat (file-name-base dir-locals-file) "-2.el")))))
    (dolist (buffer (or (ignore-errors (projectile-project-buffers))
                        (buffer-list)))
      (with-current-buffer buffer
        (when buffer-file-name
          (hack-dir-local-variables-non-file-buffer))))))

(define-hook! ymacs|after-init-hook (after-init-hook)
  ;; global-modes
  (global-font-lock-mode 1)
  (global-subword-mode 1)
  (global-page-break-lines-mode 1)
  (global-auto-revert-mode 1)

  (column-number-mode 1)
  (show-paren-mode 1)
  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  (electric-indent-mode -1)

  ;; Keep mouse at upper-right corner when typing
  ;; (mouse-avoidance-mode 'banish)
  ;; Purges buffers which haven't been displayed in 3 days
  (midnight-mode 1)
  ;; (display-time-mode 1)
  (transient-mark-mode 1)
  ;; (delete-selection-mode 1)
  ;; Auto save to files
  ;; (auto-save-visited-mode 1)

  ;; Restore `file-name-handler-alist'
  (setq file-name-handler-alist ymacs-file-name-handler-alist)
  (setq gc-cons-threshold ymacs-gc-cons-threshold)
  (setq gc-cons-percentage 0.1)

  (add-to-list 'file-name-handler-alist
               (cons ymacs-external-file-regexp #'ymacs//external-file-handler))

  (run-with-timer 1 nil #'run-hooks 'ymacs-after-init-idle-hook)
  (run-with-idle-timer ymacs-autosave-interval t #'run-hooks 'ymacs-autosave-hook)

  ;; GC all sneaky breeky like
  (add-function :after after-focus-change-function
    (lambda ()
      (unless (frame-focus-state)
        (garbage-collect))))

  ;; enabel native-compile after initialization
  ;; (setq comp-deferred-compilation t)

  (message "Init Time: %s" (emacs-init-time)))
