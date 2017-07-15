(setq history-delete-duplicates t)
;; use 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)
;; no automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; (display-time)

(setq-default buffers-menu-max-size 30
              case-fold-search t
              indicate-unused-lines nil
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
              tooltip-delay 1.5
              ;; bad idea, could accidentally edit others' code
              ;; require-final-newline t
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              visible-bell nil
              speedbar-use-images nil)

(setq projectile-completion-system 'ivy)
(setq system-time-locale "C")
(setq imenu-max-item-length 1024)
(setq minibuffer-prompt-properties
      '(;; key value
        read-only t
        point-entered minibuffer-avoid-prompt
        face minibuffer-prompt))

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq backup-by-coping t ; don't clobber symlinks
      delete-old-versions t
      version-control t  ;use versioned backups
      kept-new-versions 6
      kept-old-versions 2)

;; Donot make backups of files, not safe
(setq vc-make-backup-files nil)

;; Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Write backup files to data directory
(let ((dir (expand-file-name "~/.emacs.d/data")))
  (unless (file-exists-p dir)
    (make-directory dir)
    (if (not (file-exists-p (expand-file-name "backups" dir)))
        (make-directory (expand-file-name "backups" dir)))))

(defvar large-buffer-size 1048576)
;; default prog-mode setup
(defun generic-prog-mode-hook-setup ()
  (try-turn-on-semantic-mode)
  ;; auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  ;; eldoc, show API doc in minibuffer echo area
  (eldoc-mode 1)
  (show-paren-mode 1)
  (hs-minor-mode 1)
  (when (< (buffer-size) large-buffer-size)
    (highlight-indent-guides-mode))
  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)

;; display long lines in truncated style (end line with $)
(defun truncate-lines-setup ()
  (toggle-truncate-lines 1))
(add-hook 'grep-mode-hook 'truncate-lines-setup)

;; tab to skip close pair
(defun indent-for-tab-or-close (fn &optional arg)
  (if (looking-at "`\\|\"\\|}\\|\\$")
      (forward-char 1)
    (if (save-excursion (forward-line 0)
                        (and outline-minor-mode (looking-at-p outline-regexp)))
        (outline-toggle-children)
      (funcall fn arg))))
(advice-add 'indent-for-tab-command :around #'indent-for-tab-or-close)

;; make scratch buffer unkillable
(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; turns on auto-fill-mode, don't use text-mode-hook
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; recentf-mode
(transient-mark-mode t)
(recentf-mode 1)
(defvar recentf-can-track t)
(setq recentf-keep '((lambda (fn)
                       (and recentf-can-track
                           (or (file-remote-p fn)
                               (file-readable-p fn))))))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:"))

;; ANSI-escape coloring in compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; automatic save place of each buffer
(require 'saveplace)
(setq-default save-place t)

;; tramp setup
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

;; minibuffer-hook
(defun my-minibuffer-setup-hook ()
  ;; Use lispy in the minibuffer
  (conditionally-enable-lispy 1)
  (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (conditionally-enable-lispy -1)
  (setq gc-cons-threshold (* 100 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; history
(safe-wrap
 (progn
   (setq history-length 8000)
   (setq savehist-additional-variables
         '(search-ring regexp-search-ring kill-ring))
   (savehist-mode 1)))

(setq-default initial-scratch-message
              (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))

(with-eval-after-load 'whitespace
  (setq whitespace-style '(face spaces tabs newline
                                space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [182 10]))
  (setcdr (assoc 'tab-mark whitespace-display-mappings)
          '(9 [8594] [92 9])))

(provide 'init-defaults)