;; -*- lexical-binding: t -*-

(defun core*desktop-save-unless-loaded ($fn &rest $args)
  (if (or (called-interactively-p 'interactive)
          desktop-file-modtime)
      (apply $fn $args)
    (message "Current desktop was not loaded from a file. Ignored")))

(with-eval-after-load 'desktop
  (advice-add 'desktop-save :around #'core*desktop-save-unless-loaded)
  (add-to-list 'desktop-minor-mode-handlers '(orgtbl-mode . ignore))
  (add-to-list 'desktop-minor-mode-handlers '(hs-minor-mode . ignore))
  (add-to-list 'desktop-minor-mode-handlers '(auto-revert-mode . ignore)))

(with-eval-after-load 'bookmark
  (define-hook! core|setup-buffer-bookmark (find-file-hook)
    ;; Setup default bookmark
    (setq bookmark-current-bookmark
          (ignore-errors
            (loop for (name . record) in bookmark-alist
                  when (equal (file-truename (buffer-file-name))
                              (file-truename (bookmark-get-filename name)))
                  do (return name)))))

  (bookmark-maybe-load-default-file)
  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (core|setup-buffer-bookmark))))

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 2))

(with-eval-after-load 'xref
  (define-key! :map xref--xref-buffer-mode-map
    ("j" . (lambda! () (xref--search-property 'xref-item)))
    ("k" . (lambda! () (xref--search-property 'xref-item t))))
  (define-hook! xref*xref-buffer-setup (xref--xref-buffer-mode-hook)
    (toggle-truncate-lines 1))
  (add-to-list 'xref-prompt-for-identifier
               'xref-find-references :append))

(setq flycheck-keymap-prefix (kbd "C-c f"))
(with-eval-after-load 'flycheck
  ;; Do not check during newline
  (setq-default flycheck-check-syntax-automatically
                '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix ""
        flycheck-idle-change-delay 1))

(with-eval-after-load 'hippie-exp
  (require 'hippie-exp-ext)
  (setq-default hippie-expand-try-functions-list
                '(try-complete-file-name-partially
                  try-complete-file-name
                  try-expand-dabbrev
                  try-expand-all-abbrevs
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-expand-dabbrev-limited-chars
                  try-expand-dabbrev-limited-chars
                  try-expand-dabbrev-limited-chars-all-buffers)))

(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(setq projectile-keymap-prefix (kbd "C-x p"))
(with-eval-after-load 'projectile
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-require-project-root nil)
  (setq projectile-globally-ignored-file-suffixes '(".pyc" ".elc"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)

  (add-hook 'kill-emacs-hook #'projectile-cleanup-known-projects))

(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

(with-eval-after-load 'session
  (setq session-registers '(t (48 . 57) 45 61 92 96  (97 . 122))))

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

(with-eval-after-load 'fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

;; Smart tab
(defvar core--indent-close-list '(?\} ?\$ ?\] ?\' ?\` ?\"))
(defun core*indent-for-tab ($fn &rest $arg)
  (if (save-excursion
        (forward-line 0)
        (and outline-minor-mode (looking-at-p outline-regexp)))
      ;; Toggle outline
      (outline-toggle-children)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (apply $fn $arg)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 (called-interactively-p 'interactive))
        (cond ;; Skip close paren
         ((memq (char-after) core--indent-close-list)
          (forward-char 1))
         ;; Trigger completions
         ((and (looking-back "\\(?:\\s_\\|\\sw\\)\\{2,\\}\\(?:\\.\\|->\\|::?\\)?"
                             (max (point-min) (- (point) 5)))
               (not (memq (get-text-property (- (point) 1) 'face)
                          '(font-lock-string-face font-lock-doc-face)))
               (not (eq tab-always-indent 'complete)))
          (if (bound-and-true-p company-idle-delay)
              (call-interactively 'hippie-expand)
            (call-interactively 'company-complete))))))))
(advice-add 'indent-for-tab-command :around #'core*indent-for-tab)

(defun core*desktop-read ($fn &rest $args)
  "Temporarily disable semantic mode when load desktop"
  (let ((semantic-enable-p semantic-mode))
    (semantic-mode -1)
    (apply $fn $args)
    (when semantic-enable-p
      (core/enable-semantic))))
(advice-add 'desktop-read :around #'core*desktop-read)

(define-key!
  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("C-x , ," . core/search-in-chrome)
  ("C-x , l" . display-line-numbers-mode)
  ("C-x , -" . core/copy-file-name)
  ("C-x C-b" . ibuffer)
  ("C-x C-d" . find-name-dired)

  ("C-x D" . core/delete-this-file)
  ("C-x R" . core/rename-this-file-and-buffer)
  ("C-x W" . core/copy-this-file-to-new-file)
  ("C-x c" . core/cleanup-buffer-safe)

  ("C-x w [" . winner-undo)
  ("C-x w ]" . winner-redo)

  ("M--" . er/expand-region)
  ("M-/" . hippie-expand)

  ("M-g n" . flycheck-next-error)
  ("M-g p" . flycheck-previous-error)

  ("M-i" . iedit-mode)
  ("M-s e" . core/eval-and-replace)
  ("M-s o" . core/occur-dwim)
  ("RET" . newline-and-indent)

  ([f10] . compile)
  ([f7] . core/create-scratch-buffer))

(provide 'core-misc)
