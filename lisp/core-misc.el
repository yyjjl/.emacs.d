(declare-function browse-url-chrome "browse-url")



;; Default prog-mode setup
(define-hook! core|generic-prog-mode-setup (prog-mode-hook
                                            LaTeX-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (condition-case err
      (hs-minor-mode 1)
    (error (message "%s" (error-message-string err))))
  (flycheck-mode 1)
  (hl-line-mode 1)
  (when (< (buffer-size) core-large-buffer-size)
    ;; (highlight-indentation-current-column-mode 1)
    ;; (when (fboundp 'display-line-numbers-mode)
    ;;   (display-line-numbers-mode 1))
    (highlight-indentation-mode 1)
    (auto-revert-mode 1))

  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

(define-hook! core|generic-text-mode-setup (text-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  (hl-line-mode 1)
  (auto-fill-mode 1)
  (when (< (buffer-size) core-large-buffer-size)
    ;; (when (fboundp 'display-line-numbers-mode)
    ;;   (display-line-numbers-mode 1))
    (auto-revert-mode 1))
  (setq indicate-empty-lines t))

(define-hook! core|generic-comint-mode-setup (comint-mode-hook)
  (local-set-key [remap completion-at-point] #'counsel-company)

  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (setq show-trailing-whitespace nil)
  (setq-local company-idle-delay nil))




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

;; (with-eval-after-load 'display-line-numbers
;;   (setq display-line-numbers-type t)
;;   (setq display-line-numbers-widen t))

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
        flycheck-idle-change-delay 1.5))

(with-eval-after-load 'hippie-exp
  (setq-default hippie-expand-try-functions-list
                '(try-complete-file-name-partially
                  try-complete-file-name
                  try-expand-all-abbrevs
                  try-expand-dabbrev
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill)))

(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(setq projectile-keymap-prefix (kbd "C-x p"))
(with-eval-after-load 'projectile
  ;; (setq projectile-mode-line
  ;;       '(:eval (format "[%s]" (or projectile-project-name
  ;;                                  projectile-cached-project-name
  ;;                                  "-"))))
  (setq projectile-require-project-root nil)
  (setq projectile-globally-ignored-file-suffixes '(".pyc" ".elc"))
  (setq projectile-require-project-root t)
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
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s"
                         "M-o" "C-x" "C-c" "C-z"))

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
         ((and (looking-back "\\(?:\\s_\\|\\sw\\)\\{2,\\}\\(?:.\\|->\\)?"
                             (max (point-min) (- (point) 5)))
               (not (memq (get-text-property (- (point) 1) 'face)
                          '(font-lock-string-face font-lock-doc-face)))
               (not (eq tab-always-indent 'complete)))
          (call-interactively 'hippie-expand)))))))
(advice-add 'indent-for-tab-command :around #'core*indent-for-tab)

(defun core*desktop-read ($fn &rest $args)
  "Temporarily disable semantic mode when load desktop"
  (let ((semantic-enable-p semantic-mode))
    (semantic-mode -1)
    (apply $fn $args)
    (when semantic-enable-p
      (core/enable-semantic))))
(advice-add 'desktop-read :around #'core*desktop-read)

;; Delete the current file
(defun core/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

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
      (prin1 value (current-buffer))
      (activate-mark 1)
      (goto-char $start))))

(defvar core-search-engine-alist
  '(("g" "google" "http://www.google.com/search?q=%s")
    ("q" "stackoverflow" "http://www.google.com/search?q=%s+site:stackoverflow.com")
    ("w" "wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
    ("d" "dictionary" "http://dictionary.reference.com/search?q=%s")
    ("cpp" "cpp" "https://www.google.com/search?q=cpp+%s")))

(defun core%read-search-engine ()
  (assoc-string
   (car (split-string-and-unquote
         (ivy-read "Engine: "
                   (mapcar (lambda (x)
                             (format "%-6s %s"
                                     (nth 0 x)
                                     (propertize (nth 2 x)
                                                 'face font-lock-comment-face)))
                           core-search-engine-alist)
                   :preselect "g"
                   :matcher (lambda (re candidates)
                              (when (and (stringp re)
                                         (not (string-prefix-p "^" re)))
                                (setq re (concat "^" re)))
                              (ivy--re-filter re candidates))
                   :require-match t)))
   core-search-engine-alist))

(defun core/search-in-chrome ($engine $keyword)
  (interactive
   (let ((engine (core%read-search-engine)))
     (list (nth 2 engine)
           (read-from-minibuffer (concat (nth 1 engine) ": ")
                                 nil nil 'core-search-history))))
  (unless (featurep 'browse-url)
    (require 'browse-url))
  (browse-url-chrome (format $engine $keyword))
  (run-hooks 'core-after-search-hook))

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
  ("C-x R" . core/rename-this-file-and-buffer)
  ("C-x D" . core/delete-this-file)
  ("C-x C-d" . find-name-dired)
  ("C-x W" . core/copy-this-file-to-new-file)
  ("C-x c" . core/cleanup-buffer-safe)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("C-x C-b" . ibuffer)
  ("C-x ," . core/search-in-chrome)
  ("M-/" . hippie-expand)

  ("M--" . er/expand-region)

  ("M-i" . iedit-mode)
  ("M-s o" . core/occur-dwim)
  ("M-s e" . core/eval-and-replace)

  ("RET" . newline-and-indent)

  ("C-}" . core/company-yasnippet)
  ("C-c TAB" . company-complete)
  ("C-c <tab>" . company-complete)
  ([f6] . core/toggle-company-ispell)
  ([f7] . core/create-scratch-buffer)
  ([f10] . compile)
  ("C-<up>" . text-scale-increase)
  ("C-<down>" . text-scale-decrease)

  ("C-x w [" . winner-undo)
  ("C-x w ]" . winner-redo)

  ("M-g n" . flycheck-next-error)
  ("M-g p" . flycheck-previous-error))

(provide 'core-misc)
