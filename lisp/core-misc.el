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

(config! desktop
  :advice
  (:around desktop-save
   :use save-unless-loaded
   :define (-fn &rest -args)
   (if (or (called-interactively-p 'interactive)
           desktop-file-modtime)
       (apply -fn -args)
     (message "Current desktop was not loaded from a file. Ignored")))

  :config
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
                  flycheck-mode
                  hs-minor-mode
                  auto-revert-mode
                  lsp-mode))
    (add-to-list 'desktop-minor-mode-table (cons mode nil))))

(config! bookmark
  :hook
  (setup-buffer
   :define (find-file-hook)
   (unless (file-remote-p default-directory)
     ;; Setup default bookmark
     (setq bookmark-current-bookmark
           (ignore-errors
             (cl-loop for (name . record) in bookmark-alist
                      when (equal (file-truename (buffer-file-name))
                                  (file-truename (bookmark-get-filename name)))
                      return name)))))

  :config
  (bookmark-maybe-load-default-file)
  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (bookmark|setup-buffer))))

(config! ffap
  :advice (:around ffap-guesser :name ignore-errors!)
  ;; do not use ping, it's very slow
  :config (setq ffap-machine-p-known 'reject))

(config! which-key
  :init
  (setq which-key-dont-use-unicode t)

  :config
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-show-remaining-keys t)

  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c ," "semantic")
  (which-key-add-key-based-replacements "C-c @" "hide-show")
  (which-key-add-key-based-replacements "C-c f" "flycheck")
  (which-key-add-key-based-replacements "C-c i" "counsel")
  (which-key-add-key-based-replacements "C-c m" "mc")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x j" "jump")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "register & rectangle")
  (which-key-add-key-based-replacements "C-x t" "tab & hide-show")
  (which-key-add-key-based-replacements "C-x w" "winner & buf-move & ivy-view")
  (which-key-add-key-based-replacements "C-x g" "git")
  (which-key-add-key-based-replacements "C-x ," "hydra & misc")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode "C-c ?" "checkdoc")
  (which-key-add-major-mode-key-based-replacements 'python-mode "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c C-v" "babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode "C-c t" "table")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode "C-c C-x" "markdown-toggle"))

(config! winum
  :bind
  (:map winum-keymap
   ("C-x w")
   ("M-0" . core/goto-next-char-or-minibuffer)
   ("M-1" . winum-select-window-1)
   ("M-2" . winum-select-window-2)
   ("M-3" . winum-select-window-3)
   ("M-4" . winum-select-window-4)
   ("M-5" . winum-select-window-5)
   ("M-6" . winum-select-window-6))

  :config
  (add-to-list 'winum-ignored-buffers " *LV*")

  (setcdr (assoc 'winum-mode minor-mode-map-alist) winum-keymap)

  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local
        winum-reverse-frame-list nil
        winum-auto-assign-0-to-minibuffer t))

(config! display-line-numbers
  :config
  (setq display-line-numbers-type t)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(config! whitespace
  :config
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style '(face tabs tab-mark spaces space-mark empty)))

(config! view
  :bind
  (:map view-mode-map
   ("s" . swiper/dispatch)
   ("q" . View-exit)
   ("Q" . View-quit)))

(config! xref
  :bind
  (:map xref--xref-buffer-mode-map
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("j" . (lambda! () (xref--search-property 'xref-item)))
   ("k" . (lambda! () (xref--search-property 'xref-item t))))

  :hook
  (:anonymous
   :define (xref--xref-buffer-mode-hook)
   (toggle-truncate-lines 1))

  :advice
  (:around xref-find-definitions
   :define (-fn -identifier)
   (condition-case err
       (funcall -fn -identifier)
     (user-error
      (if-let* ((symbol (thing-at-point 'symbol)))
          (counsel-rg symbol)
        (message "%s" (error-message-string err))))))

  :config
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(config! grep
  :config
  (setq grep-highlight-matches t)
  (setq grep-scroll-output t)
  (dolist (v core-ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))

(config! flymake
  :bind
  (:map flymake-mode-map ("C-c f l" . flymake-show-diagnostics-buffer))
  (:map flymake-diagnostics-buffer-mode-map
   ("n" . next-line)
   ("j" . next-line)
   ("p" . previous-line)
   ("k" . previous-line))

  :hook
  (setup
   :define (flymake-mode-hook)
   (if flymake-mode
       (progn
         (put 'next-error-function 'flymake-old-next-error-function next-error-function)
         (setq next-error-function 'flymake-goto-next-error))
     (setq next-error-function (get 'next-error-function 'flymake-old-next-error-function)))))

(config! flycheck
  :bind (:map flycheck-command-map ("j" . counsel/flycheck))
  :init
  (setq flycheck-keymap-prefix (kbd "C-c f"))

  :advice
  (:around flycheck-add-overlay
   :define (-fn &rest -args)
   (let ((ov (apply -fn -args)))
     (overlay-put ov 'priority -10)
     ov))

  :config
  ;; Do not check during newline
  (setq-default flycheck-checker-error-threshold 400)
  (setq-default flycheck-check-syntax-automatically '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix "")
  (setq flycheck-idle-change-delay 1))

(config! hippie-exp
  :init
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

(config! projectile
  :prefix core
  :bind (:map projectile-mode-map ("C-x p" :map projectile-command-map))

  :hook (projectile-cleanup-known-projects (kill-emacs-hook))

  :advice
  (:after projectile-invalidate-cache
   :define (_)
   (dolist (buffer (ignore-errors (projectile-project-buffers)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (dolist (var core-projectile-invalidate-cache-empty-vars)
           (set var nil))))))

  (:override projectile-run-compilation
   :define (-cmd)
   (if (functionp -cmd)
       (funcall -cmd)
     (compile -cmd t)))

  (:override projectile-edit-dir-locals
   :define (&optional -directory)
   "Edit or create a .dir-locals.el file of the project."
   (interactive (list (let ((root (projectile-project-root)))
                        (or (and current-prefix-arg
                                 (read-directory-name "Select root" root))
                            root))))
   (let ((default-directory -directory))
     (condition-case nil
         (call-interactively #'add-dir-local-variable)
       (quit
        (when-let ((files (dir-locals--all-files -directory)))
          (if (= (length files) 1)
              (find-file (car files))
            (find-file (ivy-read "Open file: " files :require-match t))))))))

  ;; Projectile root-searching functions can cause an infinite cl-loop on TRAMP
  ;; connections, so disable them.
  (:around projectile-locate-dominating-file :name ignore-remote!)

  :config
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
  (setq projectile-enable-caching (not noninteractive)))

(config! yasnippet
  :advice
  (:around yas-next-field-or-maybe-expand
   :use expand-local-snippets
   :define (-fn &rest -args)
   (or (core//try-expand-local-snippets)
       (apply -fn -args)))

  :config
  (config! org
    :advice (:around org-cycle :name yasnippet*around-expand-local-snippets))

  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))

(config! isearch
  :bind (:map isearch-mode-map ("C-o" . isearch-occur)))

(config! recentf
  :hook (recentf-save-list (core-autosave-hook))

  :config
  (defun core//recentf-keep? (-fn)
    (and core--recentf-enabled-p
         ;; The order must be kept
         (or (file-remote-p -fn)
             (and (file-readable-p -fn)
                  (file-writable-p -fn)))))

  (setq recentf-keep '(core//recentf-keep?))
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude (list "/tmp/" "^/sshx?:" "/sudo:" "\\.elc$"
                              "/node_modules/"
                              "\\.\\(gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "/TAGS$"
                              (expand-file-name "~/downloads")
                              emacs-var-direcotry)))
(config! session
  :hook (session-save-session (core-autosave-hook))

  :config
  (setq session-globals-max-size 500)
  (setq session-globals-include '((file-name-history 500)
                                  (kill-ring 50)
                                  (session-file-alist 500 t)
                                  search-ring
                                  regexp-search-ring))
  (setq session-registers '(t (48 . 57) 45 61 92 96 (97 . 122))))

(config! tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (setq tramp-terminal-type "tramp")
  (setq tramp-default-method "ssh")
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (file-remote-p name 'method)))))
  (setq tramp-chunksize 8192)
  (setq tramp-verbose 1)
  ;; @see https://github.com/syl20bnr/spacemacs/issues/1921
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(config! fcitx
  :config ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))



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
(defun core//get-occur-buffer ()
  (cl-loop
   for window in (window-list)
   for buffer = (window-buffer window)
   when (with-current-buffer buffer
          (apply 'derived-mode-p
                 core-auto-next-error-buffer-derived-modes))
   return buffer))

(defun core*around-next-error (-fn &rest -args)
  (let ((occur-buffer (core//get-occur-buffer)))
    (if-let (window (and occur-buffer (get-buffer-window occur-buffer)))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args))))

(advice-add 'next-error :around  #'core*around-next-error)

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
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)

  ("C-x C-_" . session-jump-to-last-change)

  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-x G" . revert-buffer)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)

  ("M--" . er/expand-region)
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
  ([f9] . core/run-current-file))

(define-key! :map indent-rigidly-map
  ("[" . indent-rigidly-left)
  ("]" . indent-rigidly-right)
  ("{" . indent-rigidly-left-to-tab-stop)
  ("}" . indent-rigidly-right-to-tab-stop))

(define-key! :map special-mode-map
  ("u" . scroll-down-command)
  ("y" . scroll-down-line)
  ("e" . scroll-up-line))

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)

(provide 'core-misc)
