(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

;;* Load Path
;; Add site-package's path to `load-path'
(when (fboundp 'normal-top-level-add-to-load-path)
  (dolist (dir (directory-files emacs-private-directory))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir emacs-private-directory))))
  (add-to-list 'load-path emacs-private-directory))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;;* Default Values
;; No tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
;; Do not show mode-line until setup finished
(setq-default mode-line-format nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

 ;; Silence advised function warnings
(setq-default ad-redefinition-action 'accept)
 ;; Make `apropos' more useful
(setq-default apropos-do-all t)
 ;; kill compilation process before starting another
(setq-default compilation-always-kill t)
(setq-default compilation-skip-threshold 2)
(setq-default compilation-scroll-output t)
(setq-default confirm-nonexistent-file-or-buffer t)
(setq-default delete-by-moving-to-trash t)
(setq-default enable-recursive-minibuffers nil)
;; Update ui less often
(setq-default idle-update-delay 2)
 ;; keep the point out of the minibuffer
(setq-default mark-ring-max 128)
(setq-default minibuffer-prompt-properties
              '(read-only t point-entered minibuffer-avoid-prompt
                          face minibuffer-prompt))
;; History & backup settings
(setq-default auto-save-default t)
(setq-default auto-save-timeout 8)
(setq-default create-lockfiles nil)
(setq-default history-length 500)
(setq-default history-delete-duplicates t)
(setq-default make-backup-files nil)
 ;; No automatic new line when scrolling down at buffer bottom
(setq-default next-line-add-newlines nil)
(setq-default buffers-menu-max-size 30)
(setq-default case-fold-search t)
(setq-default compilation-scroll-output t)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default save-interprogram-paste-before-kill t)
(setq-default indent-tabs-mode nil)
 ;; `line-spacing' make inline-image flickering a lot
(setq-default line-spacing 0.25)
(setq-default mouse-yank-at-point t)
(setq-default set-mark-command-repeat-pop t)
(setq-default echo-keystrokes 0.25)
;; Bad idea, could accidentally edit others' code
;; (setq-default                require-final-newline t)
(setq-default tooltip-delay 0.5)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows 50)
(setq-default speedbar-use-images nil)
(setq-default large-file-warning-threshold (* 512 1024 1024))
(setq-default line-number-display-limit core-large-buffer-size)
(setq-default system-time-locale "C")
(setq-default imenu-max-item-length 1024)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)
(setq-default backup-by-coping t)
(setq-default delete-old-versions t)
;; Use versioned backups
(setq-default version-control t)
(setq-default kept-new-versions 6)
(setq-default kept-old-versions 2)
(setq-default select-enable-clipboard t)
(setq-default select-enable-primary t)
(setq-default fill-column 79)
(setq-default desktop-save 'ask-if-new)
;; Scrolling
(setq-default auto-window-vscroll nil)
(setq-default scroll-conservatively 0)
(setq-default scroll-preserve-screen-position t)

(setq-default vc-make-backup-files nil)
;; increase process buffer
(setq read-process-output-max (* 2 1024 1024))

;; be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq initial-scratch-message
      (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))

;;* Recentf
(defun core//recentf-keep? (-fn)
  (and core--recentf-enabled-p
       ;; The order must be kept
       (or (file-remote-p -fn)
           (and (file-readable-p -fn)
                (file-writable-p -fn)))))
(setq recentf-keep '(core//recentf-keep?))
(setq recentf-max-saved-items 2048
      recentf-exclude (list "/tmp/" "^/sshx?:" "/sudo:" "\\.elc$"
                            "/node_modules/"
                            "\\.\\(gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "/TAGS$"
                            (expand-file-name "~/downloads")
                            emacs-var-direcotry))

;;* Savehist
;; (setq history-length 1000)
;; (setq savehist-additional-variables '(ivy-views))
;; (ignore-errors (savehist-mode 1))

(defvar core-autosave-interval 300)
(defvar core-autosave-hook nil)

(defsubst core//save-variable (symbol file)
  (let ((real-file (expand-var! file))
        (val (default-value symbol)))
    (with-temp-buffer
      (insert (format "(setq-default %s " symbol)
              (if (consp val) "'" "")
              (prin1-to-string (default-value symbol))
              ")")
      (write-region (point-min) (point-max) real-file))))

(defsubst core//load-variable (symbol file)
  (let ((real-file (expand-var! file)))
    (when (file-exists-p real-file)
      (load real-file :noerror))))

;;* Handle External Files
(defun core//external-file-handler (-op &rest -args)
  (let ((file (expand-file-name (car -args))))
    (cond ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument file))))
          ((eq system-type 'gnu/linux)
           (let ((process-connection-type nil))
             (recentf-push file)
             (start-process "external-process" nil "xdg-open" file))))
    (kill-buffer)
    (let (debug-on-error)
      (error "Opened %s in external program" (file-name-nondirectory file)))))

(put 'core//external-file-handler 'safe-magic t)
(put 'core//external-file-handler 'operations '(insert-file-contents))

(defvar core--external-file-extensions
  '("pdf" "djvu" "dvi" "od[fgpst]" "docx?" "xlsx?"
    "pptx?" "mkv" "avi" "mp4" "rmvb"))
(defvar core--external-file-regexp
  (eval-when-compile
    (concat "\\.\\(?:"
            (string-join
             (append (mapcar #'upcase core--external-file-extensions)
                     core--external-file-extensions)
             "\\|")
            "\\)\\'")))

;;* Generic hooks
(define-hook! core|minibuffer-setup (minibuffer-setup-hook)
  (setq line-spacing 4)

  (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))

(define-hook! core|minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold emacs-gc-cons-threshold))

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun core//skip-special-buffers (buffer)
  (or (buffer-file-name buffer)
      (not (string-prefix-p "*" (buffer-name buffer)))))

(define-hook! (core|default-frame-setup frame) (after-make-frame-functions)
  (set-frame-parameter frame 'buffer-predicate 'core//skip-special-buffers))

;; Make scratch buffer un-killable
(define-hook! core|unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*task*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

;; Display long lines in truncated style (end line with $)
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines t)))
;; Turns on `auto-fill-mode', don't use `text-mode-hook'
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; ANSI-escape coloring in compilation-mode
(setq compilation-environment '("TERM=xterm-256color"))
(autoload 'ansi-color-apply-on-region "ansi-color")
(define-hook! core|colorize-compilation-buffer (compilation-filter-hook)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(define-hook! (core|compilation-finish-hook -buffer _)
  (compilation-finish-functions)
  (when (buffer-live-p -buffer)
    (with-current-buffer -buffer
      (unless (eq major-mode 'compilation-mode)
        ;; Sometime it will open a comint buffer
        (compilation-mode)
        ;; Record window as popup window
        (core-popups//push-window (get-buffer-window -buffer)
                                  -buffer
                                  :autoclose)))))

;; Default prog-mode setup
(define-hook! core|generic-prog-mode-setup (prog-mode-hook
                                            LaTeX-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (condition-case err
      (hs-minor-mode 1)
    (error (message "%s" (error-message-string err))))

  (hl-line-mode 1)

  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

(define-hook! core|generic-text-mode-setup (text-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (auto-fill-mode 1)
  (hl-line-mode 1)

  (setq indicate-empty-lines t))

(define-hook! core|generic-comint-mode-setup (comint-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (setq show-trailing-whitespace nil)
  (setq-local company-idle-delay nil))

(define-hook! core|hack-local-variables (after-save-hook)
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

(define-hook! core|after-init-hook (after-init-hook)
  ;; global-modes
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
  (setq file-name-handler-alist emacs-file-name-handler-alist
        gc-cons-threshold emacs-gc-cons-threshold
        gc-cons-percentage 0.1)

  (add-to-list 'file-name-handler-alist
               (cons core--external-file-regexp #'core//external-file-handler))

  ;; Load private configuration
  (when (file-exists-p custom-file)
    (load (file-name-sans-extension custom-file)))

  (run-with-timer 1 nil #'run-hooks 'after-init-idle-hook)
  (run-with-idle-timer core-autosave-interval t #'run-hooks 'core-autosave-hook)

  ;; GC all sneaky breeky like
   (if (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function
                     (lambda ()
                       (unless (frame-focus-state)
                         (garbage-collect))))
     (add-hook 'focus-out-hook 'garbage-collect))
  (message "Init Time: %s" (emacs-init-time)))

(provide 'core-defaults)
