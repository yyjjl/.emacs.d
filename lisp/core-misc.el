;; -*- lexical-binding: t -*-

(defcustom core-project-rsync-remote-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'stringp)

(defcustom core-project-rsync-local-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'stringp)

(defcustom core-project-rsync-extra-options nil
  "."
  :group 'projectile
  :type 'directory
  :safe (lambda (x) (and (listp x) (-all? #'stringp x))))

(defvar core-project-rsync-command
  "rsync -azh --progress --filter=':- .gitignore' %s . %s")


(defvar core-current-desktop-name nil)
(defvar core-view-code-modes
  '((lispy-mode rainbow-delimiters-count-mode)
    (t display-line-numbers-mode
       view-mode
       highlight-indentation-current-column-mode
       highlight-indentation-mode)))

(define-minor-mode core-view-code-mode
  "View code"
  :init-value nil
  (let ((switch (if core-view-code-mode 1 -1)))
    (cl-loop for (condition . modes) in core-view-code-modes
             when (or (eq condition t)
                      (and (symbolp condition)
                           (symbol-value condition))
                      (ignore-errors (eval condition t)))
             do (dolist (mode modes)
                  (funcall mode switch)))))

(defun core*desktop-save-unless-loaded (-fn &rest -args)
  (if (or (called-interactively-p 'interactive)
          desktop-file-modtime)
      (apply -fn -args)
    (message "Current desktop was not loaded from a file. Ignored")))

(with-eval-after-load 'desktop
  (advice-add 'desktop-save :around #'core*desktop-save-unless-loaded)
  ;; These minor modes are enabled by core
  (dolist (mode '(git-gutter-mode
                  ivy-mode
                  counsel-mode
                  projectile-mode
                  yas-minor-mode
                  company-mode
                  which-key-mode
                  subword-mode
                  global-auto-revert-mode
                  company-posframe-mode
                  flycheck-mode
                  hs-minor-mode
                  auto-revert-mode
                  lsp-mode))
    (add-to-list 'desktop-minor-mode-table (cons mode nil))))

(with-eval-after-load 'bookmark
  (define-hook! core|setup-buffer-bookmark (find-file-hook)
    (unless (file-remote-p default-directory)
      ;; Setup default bookmark
      (setq bookmark-current-bookmark
            (ignore-errors
              (cl-loop for (name . record) in bookmark-alist
                       when (equal (file-truename (buffer-file-name))
                                   (file-truename (bookmark-get-filename name)))
                       return name)))))

  (bookmark-maybe-load-default-file)
  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (core|setup-buffer-bookmark))))

(with-eval-after-load 'winum
  (define-key! :map winum-keymap
    ("C-x w")
    ("M-`" . winum-select-window-by-number)
    ("M-0" . core/goto-next-char-or-minibuffer)
    ("M-1" . winum-select-window-1)
    ("M-2" . winum-select-window-2)
    ("M-3" . winum-select-window-3)
    ("M-4" . winum-select-window-4)
    ("M-5" . winum-select-window-5)
    ("M-6" . winum-select-window-6))
  (setcdr (assoc 'winum-mode minor-mode-map-alist)
          winum-keymap)

  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local
        winum-reverse-frame-list nil
        winum-auto-assign-0-to-minibuffer t))

(with-eval-after-load 'ediff
  ;; do not use multi-frames
  (advice-add 'ediff-window-display-p :override #'ignore))

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(with-eval-after-load 'whitespace
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style '(face tabs tab-mark spaces space-mark empty)))

(with-eval-after-load 'view
  (define-key! :map view-mode-map
    ("s" . swiper/dispatch)
    ("q" . View-exit)
    ("Q" . View-quit)))

(with-eval-after-load 'xref
  (define-key! :map xref--xref-buffer-mode-map
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("j" . (lambda! () (xref--search-property 'xref-item)))
    ("k" . (lambda! () (xref--search-property 'xref-item t))))

  (add-hook 'xref--xref-buffer-mode-hook (lambda () (toggle-truncate-lines 1)))

  (defun xref*around-xref-find-definitions (-fn -identifier)
    (condition-case err
        (funcall -fn -identifier)
      (user-error
       (if-let* ((symbol (thing-at-point 'symbol)))
           (counsel-rg symbol)
         (message "%s" (error-message-string err))))))

  (advice-add 'xref-find-definitions :around #'xref*around-xref-find-definitions)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(defun core*flymake-eldoc-function ()
  (let ((diags (flymake-diagnostics (point))))
    (when diags
      (eldoc-message (mapconcat #'flymake-diagnostic-text diags "\n")))))

(define-hook! core|setup-flymake-mode (flymake-mode-hook)
  (if flymake-mode
      (progn
        (put 'next-error-function 'flymake-old-next-error-function
             next-error-function)
        (setq next-error-function 'flymake-goto-next-error)
        (add-function :before-until (local 'eldoc-documentation-function)
                      #'core*flymake-eldoc-function))
    (setq next-error-function (get 'next-error-function
                                   'flymake-old-next-error-function))
    (remove-function (local 'eldoc-documentation-function)
                     #'core*flymake-eldoc-function)))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c f l")
    #'flymake-show-diagnostics-buffer)

  (define-key! :map flymake-diagnostics-buffer-mode-map
    ("n" . next-line)
    ("j" . next-line)
    ("p" . previous-line)
    ("k" . previous-line)))

(setq flycheck-keymap-prefix (kbd "C-c f"))
(with-eval-after-load 'flycheck
  ;; Do not check during newline
  (setq-default flycheck-checker-error-threshold 400)
  (setq-default flycheck-check-syntax-automatically
                '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix ""
        flycheck-idle-change-delay 1))

(with-eval-after-load 'flycheck-posframe
  (setq flycheck-posframe-buffer " *flycheck-posframe-buffer*")
  (setq flycheck-posframe-warning-prefix "[W] ")
  (setq flycheck-posframe-error-prefix "[E] ")
  (setq flycheck-posframe-info-prefix "[I] ")
  (custom-set-faces
   '(flycheck-posframe-warning-face ((t :inherit warning)))
   '(flycheck-posframe-info-face ((t :foreground "#b6e63e")))
   '(flycheck-posframe-error-face ((t :inherit error)))))

(with-eval-after-load 'hippie-exp
  (setq he-dabbrev-chars "0-9a-zA-Z\\?!_")
  (setq-default hippie-expand-try-functions-list
                '(try-expand-dabbrev
                  try-expand-all-abbrevs
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-complete-file-name-partially
                  try-complete-file-name)))

(defvar core-projectile-invalidate-cache-empty-vars
  '(mode-line--cached-relative-directory
    mode-line--cached-directory
    mode-line--cached-root
    mode-line--cached-git-branch
    elpy-project-root))

(defun core*projectile-clear-vars (_)
  (dolist (buffer (ignore-errors (projectile-project-buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (var core-projectile-invalidate-cache-empty-vars)
          (set var nil))))))

(defun core*projectile-run-compilation (-cmd)
  (if (functionp -cmd)
      (funcall -cmd)
    (compile -cmd t)))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-x p") projectile-command-map)

  (advice-add 'projectile-invalidate-cache :after #'core*projectile-clear-vars)

  (advice-add 'projectile-run-compilation :override #'core*projectile-run-compilation)

  ;; Projectile root-searching functions can cause an infinite cl-loop on TRAMP
  ;; connections, so disable them.
  (advice-add #'projectile-locate-dominating-file :around #'ignore-remote!)

  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

  (setq projectile-mode-line
        '(:eval (and buffer-file-name (projectile-project-name))))
  (setq projectile-require-project-root 'prompt)
  (setq projectile-globally-ignored-file-suffixes
        '(".pyc" ".elc" ".jpg" ".png" ".svg" ".jpeg" ".pyg" ".pygtex" ".pygstyle"))
  (setq projectile-project-root-files-bottom-up
        (append '("compile_commands.json" ".ccls" ".ccls-root")
                projectile-project-root-files-bottom-up))
  (setq projectile-completion-system 'ivy)
  (setq projectile-ignored-projects '("~/" "/tmp"))
  (setq projectile-enable-caching (not noninteractive))

  (add-hook 'kill-emacs-hook #'projectile-cleanup-known-projects))

(with-eval-after-load 'yasnippet
  (defun core*expand-local-snippets (-fn &rest -args)
    (or (core//try-expand-local-snippets)
        (apply -fn -args)))

  (advice-add 'yas-next-field-or-maybe-expand
              :around #'core*expand-local-snippets)

  (with-eval-after-load 'org
    (advice-add 'org-cycle :around #'core*expand-local-snippets))

  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

(with-eval-after-load 'indent
  (define-key! :map indent-rigidly-map
    ("h" . indent-rigidly-left)
    ("l" . indent-rigidly-right)
    ("H" . indent-rigidly-left-to-tab-stop)
    ("L" . indent-rigidly-right-to-tab-stop)))

(with-eval-after-load 'session
  (add-hook 'core-autosave-hook 'session-save-session)

  (setq session-globals-max-size 500)
  (setq session-globals-include '((file-name-history 500)
                                  (kill-ring 50)
                                  (session-file-alist 500 t)
                                  search-ring
                                  regexp-search-ring))
  (setq session-registers '(t (48 . 57) 45 61 92 96 (97 . 122))))

;; `tramp' setup
(with-eval-after-load 'tramp
  (setq tramp-terminal-type "tramp")
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

(with-eval-after-load 'easy-kill
  (setq easy-kill-try-things '(url email sexp))
  (setq easy-kill-alist '((?w word " ")
                          (?s sexp "\n")
                          (?e list "\n")
                          (?L list "\n")
                          (?f filename "\n")
                          (?d defun "\n\n")
                          (?D defun-name " ")
                          (?l line "\n")
                          (?b buffer-file-name)))
  (define-key! :map easy-kill-base-map
    ("-" . easy-kill-expand)
    ("+" . easy-kill-shrink)
    ("=" . easy-kill-shrink)
    ("M-+" . easy-kill-shrink)
    ("M-=" . easy-kill-shrink)
    ("M--" . easy-kill-expand)))

;; Smart tab
(defvar core--indent-close-list '(?\} ?\$ ?\] ?\' ?\` ?\"))
(defvar core--indent-compelte-functions '(core//try-expand-local-snippets
                                          company-complete
                                          hippie-expand))
(defun core*indent-for-tab (-fn &rest -arg)
  (if (save-excursion
        (forward-line 0)
        (and outline-minor-mode (looking-at-p outline-regexp)))
      ;; Toggle outline
      (outline-toggle-children)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (apply -fn -arg)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 (called-interactively-p 'interactive)
                 (not (eq tab-always-indent 'complete)))
        (cond ;; Skip close paren
         ((memq (char-after) core--indent-close-list)
          (forward-char 1))
         ;; Trigger completions
         ((and (not (eq tab-always-indent 'complete))
               (not (memq (get-text-property (max (point-min) (1- (point))) 'face)
                          '(font-lock-string-face font-lock-doc-face)))
               (looking-back "\\(?:\\s_\\|\\sw\\)\\(?:\\.\\|->\\|::?\\)?"
                             (max (point-min) (- (point) 5))))
          ;; If company-idle-delay is nil (which means company is not trigger
          ;; automatically, <tab> will trigger it
          (catch 'done
            (dolist (func core--indent-compelte-functions)
              (when (ignore-errors (call-interactively func))
                (throw 'done nil))))))))))
(advice-add 'indent-for-tab-command :around #'core*indent-for-tab)

(defun core*desktop-read (-fn &rest -args)
  "Temporarily disable semantic mode when load desktop"
  (let ((semantic-enable-p semantic-mode))
    (semantic-mode -1)
    (apply -fn -args)
    (when semantic-enable-p
      (semantic-mode 1))))
(advice-add 'desktop-read :around #'core*desktop-read)

(defvar core-auto-next-error-buffer-derived-modes
  '(occur-mode grep-mode ivy-occur-mode xref--xref-buffer-mode compilation-mode))
(defun core*before-next-error (&rest _)
  (let ((occur-buffer
         (cl-loop
          for window in (window-list)
          for buffer = (window-buffer window)
          when (with-current-buffer buffer
                 (apply 'derived-mode-p
                        core-auto-next-error-buffer-derived-modes))
          return buffer)))
    (when (or (not next-error-last-buffer)
              (not (eq next-error-last-buffer occur-buffer)))
      (setq next-error-last-buffer occur-buffer))
    occur-buffer))

(advice-add 'next-error :before #'core*before-next-error)

(autoload 'awesome-pair-kill "awesome-pair")

(define-key! :prefix "C-x"
  ("2" . window/split-vertically)
  ("3" . window/split-horizontally)
  ("|" . window/force-split-horizontally)
  ("_" . window/force-split-vertically)
  ("?" . window/split-window-two-panel)

  (", ," . core-view-code-mode)
  (", a" . core/add-local-snippet)
  (", g" . core/search-in-chrome)
  (", -" . core/copy-file-name)
  (", c" . core/change-or-new-desktop)
  (", d" . core/delete-desktop)
  (", o" . recentf-open-files)
  ("C-b" . ibuffer)
  ("C-d" . find-name-dired)

  ("D" . core/delete-this-file)
  ("R" . core/rename-this-file-and-buffer)
  ("W" . core/copy-this-file-to-new-file)
  ("c" . core/cleanup-buffer-safe)
  ("o" . ace-window)
  ("m" . view-echo-area-messages)

  ("w [" . winner-undo)
  ("w ]" . winner-redo))

(define-key!
  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("M-k" . awesome-pair-kill)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)
  ("M--" . easy-mark)
  ("M-w" . easy-kill)
  ("M-/" . hippie-expand)
  ("M-n" . next-error)
  ("M-p" . previous-error)

  ("M-`" . other-frame)
  ("M-i" . iedit-mode)
  ("M-s e" . core/eval-and-replace)
  ("M-s o" . core/occur-dwim)
  ("RET" . newline-and-indent)

  ([C-f6] . core/display-latex-fragment-at-point)
  ([C-f7] . core/rsync-project)

  ([f10] . compile)
  ([f9] . core/run-current-file)
  ([f7] . core-view-code-mode))

(define-key! :map special-mode-map
  ("u" . scroll-down-command)
  ("y" . scroll-down-line)
  ("e" . scroll-up-line))

(provide 'core-misc)
