;; -*- lexical-binding: t; -*-

;; Add site-package's path to `load-path'
(when (file-exists-p ymacs-site-lisp-directory)
  (dolist (dir (directory-files ymacs-site-lisp-directory))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir ymacs-site-lisp-directory))))
  (add-to-list 'load-path ymacs-site-lisp-directory))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

(fset 'yes-or-no-p 'y-or-n-p)

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
(setq-default frame-title-format '("" invocation-name "@" system-name " : %b"))

(setq system-time-locale "C")
;; Set window title in xterm
(setq xterm-set-window-title t)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Make `apropos' more useful
(setq apropos-do-all t)

 ;; keep the point out of the minibuffer
(setq mark-ring-max 16)
(setq kill-ring-max 200)

;; Save clipboard contents before replacement
(setq save-interprogram-paste-before-kill t)

;; History & backup settings
(setq auto-save-default t)
(setq make-backup-files nil)
(setq history-length 500)
(setq history-delete-duplicates t)
(setq kill-do-not-save-duplicates t)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Whether confirmation is requested before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer t)
;; Use the system's trash can
(setq delete-by-moving-to-trash t)
(setq enable-recursive-minibuffers nil)

;; Vertical motion starting at end of line keeps to ends of lines
;; (setq track-eol t)

;; Don't moves point by visual lines (performace)
;; (setq-default line-move-visual nil)

;; No automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

(setq-default indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

;; Keeps  screen position if the scroll command moved  vertically out of the window
(setq scroll-preserve-screen-position t)

;; Increase process buffer
(setq read-process-output-max (* 2 1024 1024))
(setq large-file-warning-threshold (* 512 1024 1024))

(setq next-error-find-buffer-function #'ymacs-default//next-error-find-buffer)

;; be quiet at startup; don't load or display anything unnecessary
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      (format ";; Welcome to Emacs %s %s !!!"
              emacs-version
              (or user-login-name "anonymous")))

(put 'ymacs-default//external-file-handler 'safe-magic t)
(put 'ymacs-default//external-file-handler 'operations '(insert-file-contents))

(load (expand! "config-package.el") nil t)
(load (expand! "config-project.el") nil t)
(load (expand! "config-misc.el") nil t)
