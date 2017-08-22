(setq flycheck-keymap-prefix (kbd "C-c f"))
(with-eval-after-load 'flycheck
  ;; do not check during newline
  (setq-default flycheck-check-syntax-automatically
                '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix ""))

(with-eval-after-load 'session
  (add-to-list 'session-globals-include 'ivy-views))

(with-eval-after-load 'hippie-exp
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t))

(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt)))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

;; `popwin' setup
(autoload 'popwin-mode "popwin" nil t)
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap))

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
  ;; init fcitx prefix keys
  (setq fcitx-use-dbus t)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c"))

(autoload 'with-editor-export-editor "with-editor" nil nil)
(defhook main|shell-setup (shell-mode-hook eshell-mode-hook term-exec-hook)
  (with-editor-export-editor)
  (case major-mode
    (shell-mode (comint-clear-buffer))
    (term-mode (insert "clear")
               (term-send-input))))

(defhook main|after-init (after-init-hook)
  (session-initialize)

  (ivy-mode 1)
  (counsel-mode 1)
  (projectile-mode 1)
  (counsel-projectile-on)
  (which-key-mode 1)
  (yas-global-mode 1)
  ;; enable popwin-mode
  (popwin-mode 1)
  ;; global-modes
  (global-company-mode 1)
  (global-flycheck-mode 1)
  (global-subword-mode 1)
  (global-hi-lock-mode 1)
  (global-auto-revert-mode 1)
  (global-hl-line-mode 1)
  (global-page-break-lines-mode 1)

  (column-number-mode 1)
  (show-paren-mode 1)
  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  ;; `linum-mode' is slow
  ;; (global-linum-mode 1)
  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)
  ;; (global-whitespace-newline-mode 1)

  (when emacs|use-fcitx-p
    (fcitx-aggressive-setup))

  (winner-mode 1))

;; Delete the current file
(defun main|delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun main|copy-this-file-to-new-file ()
  "Copy current file to a new file without close original file."
  (interactive)
  (let* ((this (current-buffer))
         (this-name (buffer-file-name))
         (name (completing-read "New file name: "
                                #'read-file-name-internal)))
    (if (and name this-name
            (string= name this-name)
            (not (get-buffer name)))
        (message "Copy failed !!!")
      (let ((buf (get-buffer-create name)))
        (with-current-buffer buf
          (insert-buffer-substring this)
          (write-file (expand-file-name name (file-name-directory this-name))))
        (switch-to-buffer buf)))))

;; Rename the current file
(defun main|rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list  (completing-read "New file name: "
                                       #'read-file-name-internal)))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun main|copy-file-name (&optional arg)
  "Copy current file name to king ring.
If ARG = 0 copy the current directory.  If ARG > 0 copy the file
name without directory.  If ARG < 0 copy the file name without
directory and extension."
  (interactive "p")
  (let ((path (buffer-file-name)))
    (cond ((= arg 0) (setq path (buffer-file-name)))
          ((= arg 4) (setq path default-directory))
          ((= arg 16) (setq path (file-name-nondirectory path)))
          ((< arg 0) (setq path (file-name-base path))))
    (if path
        (progn (message "Copy => %s" path)
               (kill-new path))
      (message "Nothing to do"))))

(defun main|create-scratch-buffer ()
  "Create a new scratch buffer to work in.
\(could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

(defun main|cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun main|show-messages-buffer ()
  "Show message buffer."
  (interactive)
  (popup-to-buffer (get-buffer "*Messages*") nil 2.5))

(defun main|current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (message "%s => yank ring" rlt)))

(defun main|num-to-string ()
  "Convert number at point to string"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (num (and bounds (thing-at-point 'number))))
    (when num
      (goto-char (car bounds))
      (kill-region (car bounds) (cdr bounds))
      (insert num))))

(defun main|restore-files (&optional num)
  (interactive "p")
  (if recentf-mode
      (let (buf)
        (dolist (file (-take (or num 1) recentf-list))
          (setq buf (find-file-noselect file)))
        (switch-to-buffer buf))
    (message "`recentf-mode' must be turned on !!!")))

(define-keys
  ("C-x m" . main|show-messages-buffer)
  ("C-r" . isearch-backward-regexp)
  ("C-M-r" . isearch-backeard)
  ("C-x R" . main|rename-this-file-and-buffer)
  ("C-x D" . main|delete-this-file)
  ("C-x C-d" . find-name-dired)
  ("C-x W" . main|copy-this-file-to-new-file)
  ("C-x f" . main|copy-file-name)
  ("C-x c" . main|cleanup-buffer-safe)
  ("C-x ," . main|restore-files)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("C-x C-b" . ibuffer)
  ("M-/" . hippie-expand)
  ("RET" . newline-and-indent)

  ("C-}" . main|company-yasnippet)
  ("<backtab>" . company-complete)
  ([f6] . main|toggle-company-ispell)
  ([f7] . main|create-scratch-buffer)
  ("C-<up>" . text-scale-increase)
  ("C-<down>" . text-scale-decrease)

  ("C-c w [" . winner-undo)
  ("C-c w ]" . winner-redo))

(provide 'init-main-misc)
