(setq flycheck-keymap-prefix (kbd "C-c f"))
(with-eval-after-load 'flycheck
  ;; do not check during newline
  (setq-default flycheck-check-syntax-automatically
                '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix ""))

(with-eval-after-load 'hippie-exp
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy))

(with-eval-after-load 'winner
  (global-set-key (kbd "C-c w [") 'winner-undo)
  (global-set-key (kbd "C-c w ]") 'winner-redo))

(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt)))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

;; `popwin' setup
(autoload 'popwin-mode "popwin" nil t)
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap))

(with-eval-after-load 'fcitx
  ;; init fcitx prefix keys
  (setq fcitx-use-dbus t)
  (fcitx-prefix-keys-add "C-h" "M-g"))

(defhook main|after-init (after-init-hook)
  (session-initialize)
  (add-to-list 'session-globals-include 'buffer-group|groups)

  (ivy-mode 1)
  (counsel-mode 1)
  (projectile-mode 1)
  (counsel-projectile-on)
  (which-key-mode 1)
  (yas-global-mode 1)
  ;; enable popwin-mode
  (popwin-mode 1)
  ;; global-modes
  (global-linum-mode 1)
  (global-company-mode 1)
  (global-flycheck-mode 1)
  (global-subword-mode 1)
  (global-hi-lock-mode 1)
  (global-auto-revert-mode 1)
  (column-number-mode 1)

  (show-paren-mode 1)
  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)

  (global-page-break-lines-mode 1)
  ;; eldoc, show API doc in minibuffer echo area
  ;; enabled by default
  ;; (global-eldoc-mode 1)
  ;; (global-whitespace-newline-mode 1)
  ;; make zsh work correctly in emacs
  (setq system-uses-terminfo nil)

  (when emacs|use-fcitx-p
    (fcitx-aggressive-setup))

  (winner-mode 1)
  (message "Emacs setup time: %s" (emacs-init-time)))

;; Delete the current file
(defun core|delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun core|copy-this-file-to-new-file ()
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
(defun core|rename-this-file-and-buffer (new-name)
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

(defun core|copy-file-name (&optional arg)
  "Copy current file name to king ring.
If ARG = 0 copy the current directory.  If ARG > 0 copy the file
name without directory.  If ARG < 0 copy the file name without
directory and extension."
  (interactive "P")
  (let ((path (buffer-file-name)))
    (if (not path)
        (setq path default-directory)
      (when arg
        (cond ((= arg 0) (setq path (file-name-directory path)))
              ((> arg 0) (setq path (file-name-nondirectory path)))
              ((< arg 0) (setq path (file-name-base path))))))
    (if path
        (progn (message "Copy => %s" path)
               (kill-new path))
      (message "Nothing to do"))))

(defun core|create-scratch-buffer ()
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

(defun core|cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun core|show-messages-buffer ()
  "Show message buffer."
  (interactive)
  (popwin:popup-buffer (get-buffer "*Messages*")))
p
(defun core|current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (message "%s => yank ring" rlt)))

(define-keys
  ("M-s c" . aya-create)
  ("M-s y" . aya-expand)

  ("C-x m" . core|show-messages-buffer)
  ("C-r" . isearch-backward-regexp)
  ("C-M-r" . isearch-backeard)
  ("C-x R" . core|rename-this-file-and-buffer)
  ("C-x D" . core|delete-this-file)
  ("C-x W" . core|copy-this-file-to-new-file)
  ("C-x c" . core|cleanup-buffer-safe)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("C-x C-b" . ibuffer)
  ("M-/" . hippie-expand)
  ("RET" . newline-and-indent)
  ("M-'" . tiny-expand)
  ("M--" . er/expand-region)
  ("M-w" . easy-kill)

  ("C-}" . main|company-yasnippet)
  ("<backtab>" . company-complete)
  ([f6] . main|toggle-company-ispell)
  ([f7] . core|create-scratch-buffer)
  ;; ("C-=" . text-scale-increase)
  ;; ("C--" . text-scale-decrease))
  )
(provide 'init-main-misc)
