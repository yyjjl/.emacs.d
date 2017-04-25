
(setq history-delete-duplicates t)
(fset 'yes-or-no-p 'y-or-n-p)
;; no automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)
;; from robinh, time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; (display-time)
;; prolog system
(setq prolog-system 'swi)
;; regex-tool setup
(setq regex-tool-backend 'perl)
(setq-default buffers-menu-max-size 30
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
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

(setq projectile-completion-system 'ivy)
(setq system-time-locale "C")
(setq imenu-max-item-length 1024)
(setq minibuffer-prompt-properties
      '(read-only
        t
        point-entered
        minibuffer-avoid-prompt
        face
        minibuffer-prompt))

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq csv-separators '("," ";" "|" " "))

;; Write backup files to own directory
(let ((dir (expand-file-name "~/.emacs.d/data")))
  (unless (file-exists-p dir)
    (make-directory dir)
    (if (not (file-exists-p (expand-file-name "backups" dir)))
        (make-directory (expand-file-name "backups" dir)))))


(setq backup-by-coping t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/data/backups"))
      delete-old-versions t
      version-control t  ;use versioned backups
      kept-new-versions 6
      kept-old-versions 2)
;; Donot make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)

;;Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(defvar large-buffer-size 1048576)
;; default prog-mode setup
(defun generic-prog-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; auto insert closing pair
    (electric-pair-mode 1)
    ;; eldoc, show API doc in minibuffer echo area
    (eldoc-mode 1)
    (show-paren-mode 1)
    (hs-minor-mode 1)
    (when (< (buffer-size) large-buffer-size)
      (highlight-indentation-mode))
    ;; show trailing spaces in a programming mode
    (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)

;; {{ display long lines in truncated style (end line with $)
(defun truncate-lines-setup ()
  (toggle-truncate-lines 1))
(add-hook 'grep-mode-hook 'truncate-lines-setup)
;; }}

(defalias 'perl-mode 'cperl-mode)

(global-set-key [escape] 'keyboard-escape-quit)
;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(defun indent-for-tab-or-close (fn &optional arg)
  (if (looking-at "`\\|\"\\|}\\|\\$")
      (forward-char 1)
    (funcall fn arg)))

(advice-add 'indent-for-tab-command :around #'indent-for-tab-or-close)

(provide 'init-defaults)