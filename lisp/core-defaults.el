;; Add site-package's path to `load-path'
(if (fboundp 'normal-top-level-add-to-load-path)
    (dolist (-dir (directory-files emacs-site-packages-directory))
      (unless (string-match "^\\." -dir)
        (push (expand-file-name -dir emacs-site-packages-directory)
              load-path))))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; NO tool bar or scroll bar
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

(setq-default
 ;; silence advised function warnings
 ad-redefinition-action 'accept
 ;; make `apropos' more useful
 apropos-do-all t
 ;; kill compilation process before starting another
 compilation-always-kill t
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 ;; update ui less often
 idle-update-delay 2
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 history-delete-duplicates t
 make-backup-files nil
 ;; No automatic new line when scrolling down at buffer bottom
 next-line-add-newlines nil
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 save-interprogram-paste-before-kill t
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 tooltip-delay 1
 ;; Bad idea, could accidentally edit others' code
 ;; require-final-newline t
 truncate-lines nil
 truncate-partial-width-windows nil
 speedbar-use-images nil
 large-file-warning-threshold (* 512 1024 1024)

 system-time-locale "C"
 imenu-max-item-length 1024

 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 backup-by-coping t
 delete-old-versions t
 ;; Use versioned backups
 version-control t
 kept-new-versions 6
 kept-old-versions 2

 select-enable-clipboard t
 select-enable-primary t

 vc-make-backup-files nil)

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq display-time-24hr-format t
      display-time-day-and-date t)
(setq initial-scratch-message
      (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))

(defvar core-recentf-enabled? t)
(defun core%recentf-ignore? (-fn)
  (and core-recentf-enabled?
       ;; The order must be kept
       (or (file-remote-p -fn)
           (file-readable-p -fn))
       (file-writable-p -fn)))
(setq recentf-keep '(core%recentf-ignore?))
(setq recentf-max-saved-items 2048
      recentf-exclude (list "/tmp/" "/ssh:" "/sudo:" emacs-var-direcotry))

;; Purges buffers which haven't been displayed in 3 days
(midnight-mode 1)
;; Automatic save place of each buffer
(save-place-mode 1)
;; (display-time-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;; Make scratch buffer un-killable
(define-hook! core|unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

;; Display long lines in truncated style (end line with $)
(define-hook! core|truncate-lines-setup (grep-mode-hook)
  (toggle-truncate-lines 1))

;; Tab to skip close pair
(defun core%indent-for-tab ($fn &optional $arg)
  (if (looking-at "`\\|'\\|\"\\|}\\|\\$")
      (forward-char 1)
    (if (save-excursion
          (forward-line 0)
          (and outline-minor-mode (looking-at-p outline-regexp)))
        (outline-toggle-children)
      (funcall $fn $arg))))
(advice-add 'indent-for-tab-command :around #'core%indent-for-tab)

;; Turns on `auto-fill-mode', don't use `text-mode-hook'
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; ANSI-escape coloring in compilation-mode
(setq compilation-environment '("TERM=xterm-256color"))
(require 'ansi-color)
(define-hook! core/colorize-compilation-buffer (compilation-filter-hook)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(define-hook! core|minibuffer-setup (minibuffer-setup-hook)
  (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))

(define-hook! core|minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold emacs-gc-cons-threshold))

(define-hook! core|after-init-hook (after-init-hook)
  ;; Restore `file-name-handler-alist'
  (setq file-name-handler-alist emacs-file-name-handler-alist
        gc-cons-threshold emacs-gc-cons-threshold
        gc-cons-percentage 0.1)
  ;; Load private configuration
  (ignore-errors (load-file custom-file))
  (run-with-timer 1 nil
                  (lambda () (find-file (expand-var! "org/*note*")))))



(provide 'core-defaults)
