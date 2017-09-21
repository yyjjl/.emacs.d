;; Default prog-mode setup
(define-hook! core|generic-prog-mode-setup (prog-mode-hook
                                            LaTeX-mode-hook)
  (company-mode 1)
  (hs-minor-mode 1)
  (flycheck-mode 1)
  (hl-line-mode 1)
  (when (< (buffer-size) core-large-buffer-size)
    ;; (highlight-indentation-current-column-mode 1)
    (highlight-indentation-mode 1))
  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t)
  (setq  indicate-empty-lines t))

(define-hook! core|generic-text-mode-setup (text-mode-hook)
  (setq  indicate-empty-lines t)
  (hl-line-mode 1)
  (company-mode 1)
  (auto-fill-mode 1))

(setq flycheck-keymap-prefix (kbd "C-c f"))
(with-eval-after-load 'flycheck
  ;; Do not check during newline
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
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t))

(with-eval-after-load 'yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt)))

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
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s"
                         "M-o" "C-x" "C-c" "C-z"))

;; Smart tab
(defvar core--indent-close-list '(?\) ?\} ?\$ ?\] ?\' ?\` ?\"))
(defun core%indent-for-tab ($fn &rest $arg)
  (cond ((or (not (called-interactively-p 'interactive))
             (use-region-p))
         (apply $fn $arg))
        ;; Skip close paren
        ((memq (char-after) core--indent-close-list)
         (apply $fn $arg)
         (when (memq (char-after) core--indent-close-list)
           (forward-char 1)))
        ;; Toggle outline
        ((save-excursion
           (forward-line 0)
           (and outline-minor-mode (looking-at-p outline-regexp)))
         (outline-toggle-children))
        ;; Trigger completions
        ((looking-back "\\(?:\\s_\\|\\sw\\)\\{2,\\}\\(?:\\.\\|->\\)?"
                       (max (point-min) (- (point) 5)))
         (call-interactively #'hippie-expand))
        ;; Default behavior
        (t (apply $fn $arg))))
(advice-add 'indent-for-tab-command :around #'core%indent-for-tab)

;; Delete the current file
(defun core/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun core/restore-files ()
  "Find last opened file"
  (interactive)
  (if recentf-mode
      (find-file (--first (not (get-file-buffer it)) recentf-list))
    (message "`recentf-mode' must be turned on !!!")))

(defun core/copy-this-file-to-new-file ()
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
          (write-file (expand-file-name name
                                        (file-name-directory this-name))))
        (switch-to-buffer buf)))))

;; Rename the current file
(defun core/rename-this-file-and-buffer ($new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (completing-read "New file name: "
                                      #'read-file-name-internal)))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer $new-name)
        (message "A buffer named '%s' already exists!" $new-name)
      (progn
        (rename-file filename $new-name :ok-if-already-exists)
        (rename-buffer $new-name)
        (set-visited-file-name $new-name)
        (set-buffer-modified-p nil)))))

(defun core/create-scratch-buffer ()
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        (mode major-mode)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (pop-to-buffer (get-buffer-create bufname))
    (funcall mode)))

(defun core/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun core/current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (message "%s => yank ring" rlt)))

(defun core/occur-dwim ()
  (interactive)
  (if (region-active-p)
      (push (buffer-substring-no-properties
             (region-beginning)
             (region-end))
            regexp-history)
    (let ((sym (thing-at-point 'symbol)))
      (when (stringp sym)
        (push (concat "\\_<" (regexp-quote sym) "\\_>") regexp-history))))
  (call-interactively 'occur))

(defun core/eval-and-replace ($start $end)
  (interactive "r")
  (let ((value (eval
                `(let ((it (buffer-substring $start $end)))
                   ,(if (bound-and-true-p
                         mc--executing-command-for-fake-cursor)
                        (read (car read-expression-history))
                      (read--expression "Expression(it): "
                                        "(eval (read it))")))
                lexical-binding)))
    (delete-region $start $end)
    (save-excursion
      (goto-char $start)
      (print value (current-buffer))
      (activate-mark 1)
      (goto-char $start))))

(defvar socks-server '("Default server" "127.0.0.1" 1080 5))
(defun core/toggle-socket-proxy ()
  (interactive)
  (if (eq url-gateway-method 'socks)
      (let ((method (function-get #'core/toggle-socket-proxy 'method)))
        (setq url-gateway-method (or method 'native))
        (message "Use method '%s" url-gateway-method))
    (function-put #'core/toggle-socket-proxy 'method url-gateway-method)
    (setq url-gateway-method 'socks)
    (message "Use socket proxy %s" socks-server)))

(define-key!
  ("C-r" . isearch-backward-regexp)
  ("C-M-r" . isearch-backward)
  ("C-x R" . core/rename-this-file-and-buffer)
  ("C-x D" . core/delete-this-file)
  ("C-x C-d" . find-name-dired)
  ("C-x W" . core/copy-this-file-to-new-file)
  ("C-x c" . core/cleanup-buffer-safe)
  ("C-x ," . core/restore-files)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("C-x C-b" . ibuffer)
  ("M-/" . hippie-expand)
  ("M-w" . easy-kill)
  ([remap mark-sexp] . easy-mark)

  ("M-s o" . core/occur-dwim)
  ("M-i" . iedit-mode)
  ("M-s e" . core/eval-and-replace)

  ("RET" . newline-and-indent)

  ("C-}" . core/company-yasnippet)
  ("<backtab>" . company-complete)
  ([f6] . core/toggle-company-ispell)
  ([f7] . core/create-scratch-buffer)
  ("C-<up>" . text-scale-increase)
  ("C-<down>" . text-scale-decrease)

  ("C-x w [" . winner-undo)
  ("C-x w ]" . winner-redo))

(provide 'core-misc)
