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
 delete-by-moving-to-trash t
 enable-recursive-minibuffers nil
 ;; update ui less often
 idle-update-delay 2
 ;; keep the point out of the minibuffer
 mark-ring-max 128
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
 indent-tabs-mode nil
 ;; `line-spacing' make inline-image flickering a lot
 line-spacing 0.25
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 echo-keystrokes 0.25
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

;; be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)
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
(defun core%recentf-keep? ($fn)
  (and core-recentf-enabled?
       ;; The order must be kept
       (or (file-remote-p $fn)
           (and (file-readable-p $fn)
                (file-writable-p $fn)))))
(setq recentf-keep '(core%recentf-keep?))
(setq recentf-max-saved-items 2048
      recentf-exclude (list "/tmp/" "/ssh:" "/sudo:" "\\.elc$"
                            emacs-var-direcotry))

;; Purges buffers which haven't been displayed in 3 days
(midnight-mode 1)
;; (display-time-mode 1)
(transient-mark-mode 1)

(ignore-errors
  (progn
    (setq history-length 1000)
    (setq savehist-additional-variables '(ivy-views))
    (savehist-mode 1)))

(defun core%external-file-handler ($op &rest $args)
  (let ((file (expand-file-name (car $args))))
    (cond ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument file))))
          ((eq system-type 'gnu/linux)
           (let ((process-connection-type nil))
             (add-to-list 'recentf-list file)
             (start-process "external-process" nil "xdg-open" file))))
    (kill-buffer)
    (let (debug-on-error)
      (error "Opened %s in external program" (file-name-nondirectory file)))))

(put 'core%external-file-handler 'safe-magic t)
(put 'core%external-file-handler 'operations '(insert-file-contents))

(defvar core-external-file-extensions
  '("pdf" "djvu" "dvi" "od[fgpst]" "docx?" "xlsx?"
    "pptx?" "mkv" "avi" "mp4" "rmvb"))
(defvar core-external-file-regexp
  (eval-when-compile
    (concat "\\.\\(?:"
            (string-join (append (mapcar #'upcase
                                         core-external-file-extensions)
                                 core-external-file-extensions) "\\|")
            "\\)\\'")))

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Make scratch buffer un-killable
(define-hook! core|unkillable-buffer (kill-buffer-query-functions)
  (let ((bn (buffer-name)))
    (cond ((equal bn "*note*") nil)
          ((equal bn "*scratch*") (delete-region (point-min) (point-max)) nil)
          (t t))))

;; Display long lines in truncated style (end line with $)
(define-hook! core|truncate-lines-setup (grep-mode-hook)
  (toggle-truncate-lines 1))

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

  (add-to-list 'file-name-handler-alist
               (cons core-external-file-regexp
                     #'core%external-file-handler))

  ;; Load private configuration
  (ignore-errors (load-file custom-file))
  (message "Init Time: %s" (emacs-init-time)))

(provide 'core-defaults)
