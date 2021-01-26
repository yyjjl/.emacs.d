;; -*- lexical-binding: t -*-

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

(after! semantic
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (define-key! :map semantic-mode-map :prefix "C-c ,"
    ("." . semantic-ia-fast-jump)
    ("v" . semantic-ia-show-variants)
    (("TAB" "<tab>") . semantic-ia-complete-symbol))

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(;; global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          ;; global-semantic-stickyfunc-mode
          ;; Error occurs a lot
          ;; global-semantic-decoration-mode
          ;; global-semantic-highlight-func-mode
          ;; global-semantic-mru-bookmark-mode
          ))
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semanticdb-project-root-functions '(projectile-project-root))

  (dolist (mode '(c++-mode c-mode java-mode))
    (semanticdb-enable-gnu-global-databases mode)))

(after! dired
  (define-key! :map dired-mode-map
    (")" . dired-omit-mode)
    ("E" . ymacs-editor/find-file-externally)
    ("M-p" . dired-prev-subdir)
    ("M-n" . dired-next-subdir)
    (";" . dired-kill-subdir))

  (require 'dired-x)

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when ymacs-gls-path
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (if (and sys/macp (not ymacs-gls-path))
      (setq dired-listing-switches "-alh")

    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  (setq wdired-allow-to-change-permissions t))

(after! ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1048576) (format "%5.1fM" (/ (buffer-size) 1048576.0)))
     ((> (buffer-size) 1024) (format "%5.1fK" (/ (buffer-size) 1024.0)))
     (t (format "%6d" (buffer-size)))))

  (setq ibuffer-expert t
        ibuffer-use-other-window t
        ibuffer-show-empty-filter-groups nil
        ibuffer-movement-cycle nil
        ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (or (mode . dired-mode)
                        (mode . sr-mode)))
           ("Planner" (or (name . "^\\*Calendar\\*$")
                          (name . "^diary$")
                          (mode . muse-mode)
                          (mode . org-mode)
                          (mode . org-agenda-mode)))
           ("Text" (predicate . (derived-mode-p 'text-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Help\\*$")
                        (name . "^\\*info\\*$")
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Completions\\*$")
                        (name . "^\\*Compile-Log\\*$")
                        (name . "^\\*Man .*\\*$")))
           ("Emacs-Var" (filename . ".emacs.d/var"))
           ("Emacs-Config" (filename . ".emacs.d"))
           ("Code" (predicate . (derived-mode-p 'prog-mode)))
           ("Process" (or (predicate . (get-buffer-process (current-buffer)))
                          (mode . eshell-mode)))
           ("Gnus" (or (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\.newsrc-dribble"))))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only
                " " (name 18 18 :left :elide)
                " " (size-h 9 -1 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 32 -1) " " filename)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(after! projectile
  (define-key! :map projectile-mode-map
    ("C-x p" :map projectile-command-map))

  (define-key! :map projectile-command-map
    ("E" . ymacs-editor/edit-dir-locals)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project))

  ;; Use the faster searcher to handle project files
  (when ymacs-fdfind-path
    (setq projectile-generic-command
          (concat ymacs-fdfind-path " . -0 --type f --color=never")))

  (when ymacs-ctags-path
    (setq projectile-tags-command
          (eval-when-compile
            (string-join
             (cons ymacs-ctags-path (cdr (split-string projectile-tags-command)))
             " "))))

  (add-to-list 'projectile-globally-ignored-directories "__pycache__")

  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-ignored-projects '("~/" "/tmp")))

(after! ivy
  (define-key! :map ivy-minibuffer-map
    ("C-r" . ivy-reverse-i-search)
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ymacs-editor/ivy-meta-dot)
    ("C-." . ymacs-editor/ivy-meta-dot))

  (ymacs-editor//define-switch
    (swiper :doc "Swiper" :key "s")
    (swiper-isearch :doc "SwiperI" :key "x")
    (swiper-all :doc "SwiperA" :key "a")
    (ymacs-editor//rg :doc "ripgrep" :key "r" :save-text t :caller counsel-rg)
    (counsel-git-grep :doc "gitgrep" :key "g"))

  (ymacs-editor//define-switch
    (ymacs-editor//fzf :doc "fzf" :key "z" :caller counsel-fzf)
    (counsel-git :doc "git" :key "g")
    (projectile-find-file :doc "find file (project)" :key "p")
    (counsel-find-file :doc "find file" :key "f"))

  (dolist (caller '(ivy-switch-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window
                    ymacs-editor/kill-buffer))
    (ivy-configure caller
      :display-transformer-fn
      #'ymacs-editor//ivy-switch-buffer-transformer))

  (ivy-configure 'package-install
    :display-transformer-fn
    #'ymacs-editor//ivy-package-install-transformer)
  (ivy-configure 'counsel-bookmark
    :display-transformer-fn
    #'ymacs-editor//ivy-bookmark-transformer)

  (add-to-list 'ivy-hooks-alist '(t . ymacs-editor//display-help))

  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
  (setq ivy-display-functions-alist '((t . nil)))
  (setq ivy-height 13)
  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-dispatching-done-columns 3)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-on-del-error-function #'ignore))

(after! swiper
  (setq swiper-stay-on-quit t))

(after! counsel
  (add-to-list 'counsel-compile-local-builds #'ymacs-editor//default-compile-command t)

  (setq counsel-yank-pop-separator "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (setq counsel-compile-make-args "-k -j4"))

(make-variable-buffer-local 'company-backends)
(after! company
  (define-key! :map company-active-map
    ("C-d")
    ([tab] . ymacs-editor/complete-common)
    ("TAB" . ymacs-editor/complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page)
    ((0 . 9) . ymacs-editor/company-number))

  (setq-default company-backends
                `((company-capf
                   company-dabbrev-code company-keywords
                   ;; company-files
                   :with company-yasnippet
                   :separate)
                  (company-gtags company-etags)
                  company-dabbrev))

  ;; Company should be case sensitive
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-char-regexp "[0-9a-zA-Z-_]")
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-numbers t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.3)
  (setq company-require-match nil)
  ;; (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-auto-commit nil)
  ;; Not to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode
              ;; comint-mode
              ;; gud-mode
              erc-mode
              rcirc-mode shell-mode
              minibuffer-inactive-mode))

  (setf (alist-get 'company-search-mode mode-line-misc-info)
        '(("" company-search-lighter " "))))

(when ymacs-editor-use-childframe
  (after! company-posframe
    (setq company-posframe-show-indicator nil)
    (setq company-posframe-show-metadata nil)
    (setq company-posframe-quickhelp-delay nil)

    (define-key! :map company-posframe-active-map
      ("C-c C-d" . company-posframe-quickhelp-toggle)
      ("C-h" . company-posframe-quickhelp-toggle)
      ("C-v" . company-posframe-quickhelp-scroll-down)
      ("M-v" . company-posframe-quickhelp-scroll-up))))

(after! iedit
  (setq iedit-auto-narrow t))

(after! flycheck
  ;; Do not check during newline
  (setq-default flycheck-checker-error-threshold 400)
  (setq-default flycheck-check-syntax-automatically '(idle-change save mode-enabled))
  (setq flycheck-navigation-minimum-level 'warning)
  (setq flycheck-mode-line-prefix "")
  (setq flycheck-idle-change-delay 1))

(after! hideshow
  (define-key! :map hs-minor-mode-map
    ("C-x t h" (defun ymacs-editor/hs-hide-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-block))))
    ("C-x t s" (defun ymacs-editor/hs-show-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-block))))
    ("C-x t H" (defun ymacs-editor/hs-hide-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-all))))
    ("C-x t S" (defun ymacs-editor/hs-show-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-all))))
    ("C-x t l" (defun ymacs-editor/hs-hide-level ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-level))))
    ("C-x t t" (defun ymacs-editor/hs-toggle-hiding ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-toggle-hiding)))))

  (define-key! :map ymacs-editor-hs-overlay-map
    ("RET" . hs-show-block))

  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay #'ymacs-editor//hs-setup-overlay))

(after! yasnippet
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq-default yas-indent-line 'fixed)
  (setq yas-triggers-in-field nil))

(after! graphviz-dot-mode
  (require 'company-graphviz-dot)

  (remove-hook 'company-backends 'company-graphviz-dot-backend)

  (setq graphviz-dot-indent-width 4))

(after! avy
  (setq avy-keys
        '(
          ?a ?s ?d ?f ?g ?h ?j ?k ?l
          ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
          ?z ?x ?c ?v ?b ?n ?m))

  (setq avy-background t)
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt t)
  (setq avy-style 'at-full))
