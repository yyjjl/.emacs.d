;; -*- lexical-binding: t; -*-

;; Add site-package's path to `load-path'
(when (fboundp 'normal-top-level-add-to-load-path)
  (dolist (dir (directory-files ymacs-private-directory))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir ymacs-private-directory))))
  (add-to-list 'load-path ymacs-private-directory))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

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
(setq-default compilation-skip-threshold 1)
(setq-default compilation-scroll-output t)
(setq-default compilation-environment '("TERM=xterm-256color"))
(setq-default confirm-nonexistent-file-or-buffer t)
(setq-default delete-by-moving-to-trash t)
(setq-default enable-recursive-minibuffers nil)
;; Update ui less often
(setq-default idle-update-delay 2)
 ;; keep the point out of the minibuffer
(setq-default mark-ring-max 128)
(setq-default kill-ring-max 200)
 ;; Save clipboard contents before replacement
(setq-default save-interprogram-paste-before-kill t)
(setq-default minibuffer-prompt-properties
              '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
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
(setq-default tooltip-delay 0.5)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows 50)
(setq-default speedbar-use-images nil)
(setq-default large-file-warning-threshold (* 512 1024 1024))
(setq-default line-number-display-limit ymacs-large-buffer-limit)
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
;; (advice-add #'display-startup-echo-area-message :override #'ignore)
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq initial-scratch-message
      (format ";; Welcome to Emacs %s %s !!!"
              emacs-version
              (or user-login-name "anonymous")))

(autoload 'hl-fill-column-mode "hl-fill-column")

(put 'ymacs//external-file-handler 'safe-magic t)
(put 'ymacs//external-file-handler 'operations '(insert-file-contents))
