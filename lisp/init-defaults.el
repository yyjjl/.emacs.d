(setq history-delete-duplicates t)
;; use 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)
;; no automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; (display-time-mode)

(setq-default buffers-menu-max-size 30
              case-fold-search t
              indicate-unused-lines nil
              indicate-buffer-boundaries nil
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
              ;; bad idea, could accidentally edit others' code
              ;; require-final-newline t
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              visible-bell nil
              speedbar-use-images nil)

(setq system-time-locale "C")
(setq imenu-max-item-length 1024)
;; key value
(setq minibuffer-prompt-properties
      '(read-only t
        point-entered minibuffer-avoid-prompt
        face minibuffer-prompt))

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; don't clobber symbol links
(setq backup-by-coping t
      delete-old-versions t
      ;; use versioned backups
      version-control t
      kept-new-versions 6
      kept-old-versions 2)

;; Use the system clipboard
(setq select-enable-clipboard t
      select-enable-primary t)

;; Donot make backups of files, not safe
(setq vc-make-backup-files nil)

;; automatic save place of each buffer
(setq-default save-place t)
(require 'saveplace)

;; history
(setq history-length 100)
;; Use `session' can do this
;; (setq savehist-additional-variables '(search-ring regexp-search-ring))
;; (savehist-mode 1)

(setq-default initial-scratch-message
              (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))
(setq-default initial-buffer-choice (core|expand-var "org/*note*"))
;; Make scratch buffer un-killable
(defhook core|unkillable-scratch-buffer (kill-buffer-query-functions)
  (not (equal (buffer-name (current-buffer)) "*note*")))

;; recentf-mode
(transient-mark-mode t)
(recentf-mode 1)
(defvar core|recentf-enabled-p t)
(defun core|recentf-ignore-p (fn)
  (and core|recentf-enabled-p
       ;; The order must be kept
       (or (file-remote-p fn)
           (file-readable-p fn))
       (file-writable-p fn)))
(setq recentf-keep '(core|recentf-ignore-p))
(setq recentf-max-saved-items 2048
      recentf-exclude (list "/tmp/" "/ssh:" "/sudo:"
                            emacs|var-direcotry
                            (expand-file-name package-user-dir)))

;; `midnight-mode' purges buffers which haven't been displayed in 3 days
(require 'midnight)
(setq midnight-mode t)

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Display long lines in truncated style (end line with $)
(defhook core|truncate-lines-setup (grep-mode-hook)
  (toggle-truncate-lines 1))

;; tab to skip close pair
(defun core|indent-for-tab (fn &optional arg)
  (if (looking-at "`\\|\"\\|}\\|\\$")
      (forward-char 1)
    (if (save-excursion (forward-line 0)
                        (and outline-minor-mode (looking-at-p outline-regexp)))
        (outline-toggle-children)
      (funcall fn arg))))
(advice-add 'indent-for-tab-command :around #'core|indent-for-tab)

;; turns on auto-fill-mode, don't use text-mode-hook
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; ANSI-escape coloring in compilation-mode
(ignore-errors
  (setq compilation-environment '("TERM=xterm-256color"))
  (require 'ansi-color)
  (defhook core|colorize-compilation-buffer (compilation-filter-hook)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))


;; Default prog-mode setup
(defhook core|generic-prog-mode-setup (prog-mode-hook)
  (main|semantic-mode)
  (hs-minor-mode 1)
  (hl-line-mode 1)
  (when (< (buffer-size) core|large-buffer-size)
    ;; (highlight-indentation-current-column-mode 1)
    (highlight-indentation-mode 1))
  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t))

(defhook core|minibuffer-setup (minibuffer-setup-hook)
  (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))

(defhook core|minibuffer-exit (minibuffer-exit-hook)
  (setq gc-cons-threshold emacs|gc-cons-threshold))

;; `tramp' setup
(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh")
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (file-remote-p name 'method)))))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-chunksize 8192)
  (setq tramp-verbose 1)
  ;; @see https://github.com/syl20bnr/spacemacs/issues/1921
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(provide 'init-defaults)
