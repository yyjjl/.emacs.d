;;; -*- lexical-binding: t; -*

(define-advice push-mark-command (:after (&rest _) delete-dups)
  (let (new-ring)
    (dolist (marker mark-ring)
      (unless (and (markerp (car new-ring))
                   (equal (marker-position marker)
                          (marker-position (car new-ring))))
        (push marker new-ring)))
    (setq mark-ring (nreverse new-ring))))

(define-advice package-generate-autoloads (:after (-name -pkg-dir) autoclose)
  "Auto close *-autoloads.el after a package installed."
  (when-let ((name (format "%s-autoloads.el" (if (symbolp -name)
                                                 (symbol-name -name)
                                               -name)))
             (buffer (find-file-existing (expand-file-name name -pkg-dir))))
    (when-let (window (get-buffer-window buffer))
      (quit-window 'kill window))))

(define-advice package--save-selected-packages (:override (-value) dont-save)
  (when -value
    (setq package-selected-packages -value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(define-advice savehist-save
    (:around (-fn &optional -auto-save) set-additional-variables)
  (let ((variables savehist-additional-variables)
        (kill-ring (ymacs-default//filter-ring kill-ring)))
    (dolist (symbol (apropos-internal "-\\(ring\\|history\\)\\'" 'boundp))
      (unless (or (null (symbol-value symbol))
                  (memq symbol ymacs-default-savehist-exclude-variables)
                  (memq symbol savehist-minibuffer-history-variables)
                  (ring-p (symbol-value symbol))
                  (keywordp symbol))
        (cl-pushnew symbol variables)))
    (let ((savehist-additional-variables variables))
      (funcall -fn -auto-save))))

(define-advice toggle-input-method (:around (-fn &rest -args) preload)
  (if-let (item (alist-get major-mode ymacs-default-input-method-alist))
      (when (require (car item) nil t)
        (let ((default-input-method (cadr item)))
          (apply -fn -args)))
    (apply -fn -args)))

(advice-add #'ffap-guesser :around #'ignore-remote!)

(add-hook 'ediff-before-setup-hook
          (lambda () (window-configuration-to-register :ediff-windows)))
(add-hook 'ediff-quit-hook
          (lambda () (jump-to-register :ediff-windows)))

;; ANSI-escape coloring in compilation-mode
(define-hook! ymacs-default//colorize-compilation-buffer (compilation-filter-hook)
  (when (derived-mode-p 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;; Display long lines in truncated style (end line with $)
(define-hook! ymacs-default//truncate-line
  (grep-mode-hook
   compilation-mode-hook
   xref--xref-buffer-mode-hook)
  (setq truncate-lines
        (not (bound-and-true-p compilation-shell-minor-mode))))

(define-hook! ymacs-default//create-missing-directories (find-file-not-found-functions)
  "Automatically create missing directories when creating new files."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name))))
    (let ((parent-directory (file-name-directory (buffer-file-name))))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))

(define-hook! ymacs-default//minibuffer-setup (minibuffer-setup-hook)
  (setq line-spacing nil)
  (setq gc-cons-threshold most-positive-fixnum))

(define-hook! ymacs-default//minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold ymacs-gc-cons-threshold))

;; Make scratch buffer un-killable
(define-hook! ymacs-default//unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

(define-hook! ymacs-default//generic-text-mode-setup (text-mode-hook)
  (hl-line-mode 1)
  (display-fill-column-indicator-mode 1)

  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

;; Default prog-mode setup
(define-hook! ymacs-default//generic-prog-mode-setup (prog-mode-hook)
  (condition-case err
      (hs-minor-mode 1)
    (user-error (message "%s" (error-message-string err))))

  (ymacs-default//generic-text-mode-setup))

(define-hook! ymacs-default//generic-comint-mode-setup (comint-mode-hook)
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (setq show-trailing-whitespace nil)
  (setq-local company-idle-delay nil)

  (local-set-key [remap completion-at-point] #'company-complete))

(define-hook! ymacs-default//hack-local-variables (after-save-hook)
  (executable-make-buffer-file-executable-if-script-p)

  (when (and buffer-file-name
             (member (file-name-nondirectory buffer-file-name)
                     (eval-and-compile
                       (list dir-locals-file
                             (concat (file-name-base dir-locals-file) "-2.el")))))
    (hack-dir-local-variables-for-project!)))

(define-hook! ymacs-default//after-init (after-init-hook)
  ;; global-modes
  (global-subword-mode 1)
  (global-auto-revert-mode 1)

  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  (electric-indent-mode 1)

  ;; Keep mouse at upper-right corner when typing
  ;; (mouse-avoidance-mode 'banish)
  ;; Purges buffers which haven't been displayed in 3 days
  (midnight-mode 1)
  ;; (display-time-mode 1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  ;; Auto save to files
  ;; (auto-save-visited-mode 1)
  (size-indication-mode -1)
  (line-number-mode -1)
  (column-number-mode -1)

  (recentf-mode 1)
  (winner-mode 1)

  (savehist-mode 1)
  (save-place-mode 1)

  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)
  (global-so-long-mode 1)
  ;; (which-function-mode 1)

  ;; (file-name-shadow-mode 1)
  ;; Show the recursion depth in the minibuffer prompt
  (minibuffer-depth-indicate-mode 1)

  ;; Restore `file-name-handler-alist' and `gc-cons-threshold'
  (setq file-name-handler-alist
        (cons
         (cons ymacs-default-external-file-regexp
               #'ymacs-default//external-file-handler)
         ymacs-file-name-handler-alist))
  (setq gc-cons-threshold ymacs-gc-cons-threshold)
  (setq gc-cons-percentage 0.1)

  ;; Setup autoloads and packages
  (setq package-selected-packages ymacs-required-packages)

  (unless (file-exists-p ymacs-default-autoloads-file)
    (ymacs-default/generate-autoloads))

  (load ymacs-default-autoloads-file)

  (run-with-idle-timer
   ymacs-default-autosave-interval t
   #'run-hooks 'ymacs-default-autosave-hook)

  (unless (or noninteractive (daemonp))
    (require 'server)
    (unless (server-running-p)
      (server-start)))

  ;; collect garbage after focus changes
  (add-function
   :after after-focus-change-function
   (lambda ()
     (unless (frame-focus-state)
       (garbage-collect)))))
