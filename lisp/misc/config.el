;; -*- lexical-binding: t -*-

(setq he-dabbrev-chars "0-9a-zA-Z\\?!_")
(setq-default hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-all-abbrevs
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name))

(after! calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(after! isearch
  (define-key! :map isearch-mode-map
    ("C-o" . isearch-occur)))

(after! desktop
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

(after! recentf
  (defun ymacs-misc//recentf-keep-p (-fn)
    (and ymacs--buffer-visible-p
         ;; The order must be kept
         (or (file-remote-p -fn)
             (and (file-readable-p -fn)
                  (file-writable-p -fn)))))

  (setq recentf-keep '(ymacs-misc//recentf-keep-p))
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude
        (list "/tmp/" "^/sshx?:" "/sudo:" "\\.elc$"
              "/node_modules/"
              "\\.\\(gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "/TAGS$"
              ymacs-var-direcotry
              (expand-file-name package-user-dir))))

(after! bookmark
  (bookmark-maybe-load-default-file)

  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (ymacs-misc|bookmark-setup))))

(after! ffap
  ;; do not use ping, it's very slow
  (setq ffap-machine-p-known 'reject))

(after! tramp
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

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

(setq which-key-dont-use-unicode t)
(after! which-key
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

(after! display-line-numbers
  (setq display-line-numbers-type t)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(after! whitespace
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style '(face tabs tab-mark spaces space-mark empty)))

(after! view
  (define-key! :map view-mode-map
    ("s" . ymacs/swiper)
    ("q" . View-exit)
    ("Q" . View-quit)))

(after! xref
  (define-key! :map xref--xref-buffer-mode-map
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("j" . (lambda! () (xref--search-property 'xref-item)))
    ("k" . (lambda! () (xref--search-property 'xref-item t))))

  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(after! grep
  (setq grep-highlight-matches t)
  (setq grep-scroll-output t)
  (dolist (v ymacs-ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))

(after! flymake
  (define-key! :map flymake-mode-map
    ("C-c f l" . flymake-show-diagnostics-buffer))

  (define-key! :map flymake-diagnostics-buffer-mode-map
   ("n" . next-line)
   ("j" . next-line)
   ("p" . previous-line)
   ("k" . previous-line)))

(after! graphviz-dot-mode
  (require 'company-graphviz-dot)

  (remove-hook 'company-backends 'company-graphviz-dot-backend)

  (setq graphviz-dot-indent-width 4))

(setq flycheck-keymap-prefix (kbd "C-c f"))
(after! flycheck
  (define-key! :map flycheck-command-map
    ("j" . ymacs-counsel/flycheck))

  ;; Do not check during newline
  (setq-default flycheck-checker-error-threshold 400)
  (setq-default flycheck-check-syntax-automatically '(idle-change save mode-enabled))
  (setq flycheck-mode-line-prefix "")
  (setq flycheck-idle-change-delay 1))

(after! projectile
  (define-key! :map projectile-mode-map
    ("C-x p" :map projectile-command-map))

  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-globally-ignored-directories ".cache")

  (setq projectile-mode-line
        '(:eval (and buffer-file-name (projectile-project-name))))

  (setq projectile-require-project-root 'prompt)
  (setq projectile-globally-ignored-file-suffixes
        '(".pyc" ".elc" ".jpg" ".png" ".svg" ".jpeg" ".pyg"
          ".pygtex" ".pygstyle"))

  (dolist (name '("compile_commands.json" ".ccls" "Cargo.toml"))
    (add-to-list 'projectile-project-root-files-bottom-up name))

  (setq projectile-completion-system 'ivy)
  (setq projectile-ignored-projects '("~/" "/tmp"))
  (setq projectile-enable-caching (not noninteractive)))

(after! yasnippet
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))

(after! so-long
  ;; reduce false positives w/ larger threshold
  (setq so-long-threshold 1000)
  ;; make sure that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; But disable everything else that may be unnecessary/expensive for large
  ;; or wide buffers.
  (setq so-long-target-modes
        (append so-long-target-modes
                '(flycheck-mode
                  flyspell-mode
                  eldoc-mode
                  auto-composition-mode
                  hl-fill-column-mode)))
  (setq so-long-predicate #'ymacs//buffer-has-long-lines-p))

(after! skeletor
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir) (skeletor-async-shell-command "mkdir build"))))

(after! zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))

(after! prolog
  (setq prolog-system 'swi))

(after! erc-track
  (setq erc-track-enable-keybindings nil)) ;; erc

(after! pulse
  (setq pulse-delay 0.01))

(after! csv-mode
  (setq csv-separators '("," ";" "|" " "))) ;; csv

(after! inf-lisp
  (setq inferior-lisp-program "sbcl"))
