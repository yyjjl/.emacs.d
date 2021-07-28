;; -*- lexical-binding: t; -*-

;; Add site-package's path to `load-path'
(when (file-exists-p ymacs-site-lisp-directory)
  (dolist (dir (directory-files ymacs-site-lisp-directory))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir ymacs-site-lisp-directory))))
  (add-to-list 'load-path ymacs-site-lisp-directory))

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

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

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

;; be quiet at startup; don't load or display anything unnecessary
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      (format ";; Welcome to Emacs %s %s !!!"
              emacs-version
              (or user-login-name "anonymous")))

(setq enable-remote-dir-locals t)

(with-no-warnings
  (when sys/macp
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'super)
    (setq mac-function-modifier 'ctrl)))



(setq-default mode-line-buffer-identification '("%b"))

(blink-cursor-mode -1)
(tooltip-mode -1)
(window-divider-mode -1)

(setq echo-keystrokes 1)

;; paren
(setq blink-matching-paren nil)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-min-dir-content 3)

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

(ignore-errors
  (set-fringe-bitmap-face 'right-curly-arrow 'warning)
  (set-fringe-bitmap-face 'left-curly-arrow 'warning)
  (set-fringe-bitmap-face 'right-triangle 'error)
  (dolist (bitmap '(right-arrow left-arrow up-arrow down-arrow))
    (set-fringe-bitmap-face bitmap 'compilation-info)))

(setq widget-image-enable nil)

;; Don't display line number in mode line when buffer is too large
(setq line-number-display-limit ymacs-large-buffer-limit)

;; Update ui less often
(setq idle-update-delay 2)
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Disable bidirectional text for tiny performance boost
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq frame-resize-pixelwise t)

(setq cursor-in-non-selected-windows t)
(setq highlight-nonselected-windows t)

(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Minibuffer resizing
(setq-default max-mini-window-height 0.25)
(setq-default resize-mini-windows 'grow-only)

(setq split-width-threshold 120)
(setq-default line-spacing 0.1)
(setq-default truncate-lines t)

;; flash the frame to represent a bell.
(setq ring-bell-function #'ignore)
(setq visible-bell t)



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
    (setq package-selected-packages -value)))

(setq package-quickstart t)
(setq package-native-compile t)
(after! package
  (unless ymacs-package-use-gnutls-p
    (dolist (item package-archives)
      (setcdr item (replace-regexp-in-string "https:" "http:" (cdr item))))))

(package-initialize)

(require-packages!
 dash
 monokai-theme)

(require 'dash)

(setq monokai-background "#1B1D1E")
(setq monokai-height-plus-1 1.0)
(setq monokai-height-plus-2 1.0)
(setq monokai-height-plus-3 1.0)
(setq monokai-height-plus-4 1.0)

(load-theme 'monokai t)

(eval-when! ymacs-only-in-terminal-p
  (require-packages! clipetty)

  (global-clipetty-mode 1)
  (xterm-mouse-mode 1))
