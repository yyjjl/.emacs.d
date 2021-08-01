;;; -*- lexical-binding: t; -*-

(declare-function exec-path-from-shell-initialize "ext:exec-path-from-shell")

(define-advice push-mark-command (:after (&rest _) delete-dups)
  (let (new-ring)
    (dolist (marker mark-ring)
      (unless (and (markerp (car new-ring))
                   (equal (marker-position marker)
                          (marker-position (car new-ring))))
        (push marker new-ring)))
    (setq mark-ring (nreverse new-ring))))

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
        (or (ignore-errors (call-interactively #'company-files))
            (ignore-errors (call-interactively #'company-complete))
            (ignore-errors (call-interactively #'hippie-expand)))))))

(define-hook! ymacs-editor//create-missing-directories (find-file-not-found-functions)
  "Automatically create missing directories when creating new files."
  (when (buffer-file-name)
    (let ((parent-directory (file-name-directory (buffer-file-name))))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (prog1 t
             (make-directory parent-directory 'parents))))))

(define-hook! ymacs-editor//minibuffer-setup (minibuffer-setup-hook)
  (setq line-spacing nil)
  (setq gc-cons-threshold most-positive-fixnum))

(define-hook! ymacs-editor//minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold ymacs-gc-cons-threshold))

;; Make scratch buffer un-killable
(define-hook! ymacs-editor//unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

(define-hook! ymacs-editor//hack-local-variables (after-save-hook)
  (executable-make-buffer-file-executable-if-script-p)

  (when (and buffer-file-name
             (member (file-name-nondirectory buffer-file-name)
                     (eval-and-compile
                       (list dir-locals-file
                             (concat (file-name-base dir-locals-file) "-2.el")))))
    (hack-dir-local-variables-for-project!)
    (message "dir-local variables for current project are updated")))

(defun ymacs-editor//generic-setup ()
  (company-mode 1)
  (hl-line-mode 1)
  (display-fill-column-indicator-mode 1)

  (page-break-lines-mode 1)

  (when (not (derived-mode-p 'org-mode))
    (hl-todo-mode 1))

  (ymacs-editor//setup-low-level-font-lock)

  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

(define-hook! ymacs-editor//generic-text-mode-setup (text-mode-hook)
  (whitespace-mode 1)

  (ymacs-editor//generic-setup))

(define-hook! ymacs-editor//generic-prog-mode-setup (prog-mode-hook)
  (condition-case err
      (hs-minor-mode 1)
    (user-error (message "%s" (error-message-string err))))

  (ymacs-editor//generic-setup))

(after! term/tty-colors
  ;; cache for performance
  (define-advice tty-color-desc (:around (-fn -color &optional -frame) cache)
    (let ((cache-table (or (frame-parameter -frame 'tty-color-cache)
                           (let ((table (make-hash-table :test #'equal)))
                             (set-frame-parameter -frame 'tty-color-cache table)
                             table))))
      (if-let (cache (gethash (cons -color -frame) cache-table))
          (and (not (eq cache 'null)) cache)
        (let ((ret (funcall -fn -color -frame)))
          (puthash (cons -color -frame) (or ret 'null) cache-table)
          ret)))))

(run-after-init! 0
  (require 'lv)

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

  (show-paren-mode 1)

  (winner-mode 1)
  ;; Sessions
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  ;; (file-name-shadow-mode 1)
  ;; Show the recursion depth in the minibuffer prompt
  (minibuffer-depth-indicate-mode 1)

  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  (electric-indent-mode 1)

  ;; global-modes
  (semantic-mode 1)
  ;; (which-function-mode 1)
  (global-subword-mode 1)
  (global-auto-revert-mode 1)
  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)
  (global-so-long-mode 1)

  (avy-setup-default)
  (ace-pinyin-global-mode 1)
  (ace-window-display-mode 1)

  (ivy-mode 1)
  (counsel-mode 1)
  (which-key-mode 1)
  (persistent-scratch-autosave-mode 1)

  (ymacs-x//enable)
  (yas-global-mode 1)

  (eval-when! sys/macp
    (exec-path-from-shell-initialize))

  (when (and sys/linuxp (display-graphic-p -frame))
    ;; Keep mouse at upper-right corner when typing
    ;; (mouse-avoidance-mode t)

    (fcitx-aggressive-setup))

  (desktop-save-mode 1)

  (find-file-noselect (expand-cache! "org/*note*")))
