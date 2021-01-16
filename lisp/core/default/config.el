;; -*- lexical-binding: t; -*-

;; Add site-package's path to `load-path'
(when (file-exists-p ymacs-site-lisp-directory)
  (dolist (dir (directory-files ymacs-site-lisp-directory))
    (unless (string-match "^\\." dir)
      (add-to-list 'load-path (expand-file-name dir ymacs-site-lisp-directory))))
  (add-to-list 'load-path ymacs-site-lisp-directory))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

(fset 'yes-or-no-p 'y-or-n-p)

;;* Default Values
;; No tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Do not show mode-line until setup finished
(setq-default mode-line-format nil)
(setq-default frame-title-format '("" invocation-name "@" system-name " : %b"))

(setq system-time-locale "C")
;; Set window title in xterm
(setq xterm-set-window-title t)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Make `apropos' more useful
(setq apropos-do-all t)

 ;; keep the point out of the minibuffer
(setq mark-ring-max 16)
(setq kill-ring-max 200)

;; Save clipboard contents before replacement
(setq save-interprogram-paste-before-kill t)

;; History & backup settings
(setq auto-save-default t)
(setq make-backup-files nil)
(setq history-length 500)
(setq history-delete-duplicates t)
(setq kill-do-not-save-duplicates t)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Whether confirmation is requested before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer t)
;; Use the system's trash can
(setq delete-by-moving-to-trash t)
(setq enable-recursive-minibuffers t)

;; Vertical motion starting at end of line keeps to ends of lines
;; (setq track-eol t)

;; Don't moves point by visual lines (performace)
;; (setq-default line-move-visual nil)

;; No automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

(setq-default indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

;; Keeps  screen position if the scroll command moved  vertically out of the window
(setq scroll-preserve-screen-position t)

;; Increase process buffer
(setq read-process-output-max (* 2 1024 1024))
(setq large-file-warning-threshold (* 512 1024 1024))

(setq next-error-find-buffer-function #'ymacs-default//next-error-find-buffer)

;; be quiet at startup; don't load or display anything unnecessary
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      (format ";; Welcome to Emacs %s %s !!!"
              emacs-version
              (or user-login-name "anonymous")))

(put 'ymacs-default//external-file-handler 'safe-magic t)
(put 'ymacs-default//external-file-handler 'operations '(insert-file-contents))



(setq package-quickstart t)

;; The index of archive represents its priority
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("gnu" . "https://elpa.emacs-china.org/gnu/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org" . "https://elpa.emacs-china.org/org/")))

(unless ymacs-use-gnutls-p
  (dolist (item package-archives)
    (setcdr item (replace-regexp-in-string "https:" "http:" (cdr item)))))

(package-initialize)

(require-packages! dash)
(require 'dash)



(defalias 'top 'proced)

(setq he-dabbrev-chars "0-9a-zA-Z\\?!_")
(setq-default hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-all-abbrevs
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name))

(after! autorevert
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t))

(after! calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(after! compile
  (setq-default compilation-environment '("TERM=xterm-256color"))
  ;; kill compilation process before starting another
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t))

(after! isearch
  (define-key! :map isearch-mode-map
    ("C-o" . isearch-occur)))

(after! speedbar
  (setq speedbar-use-images nil))

(after! savehist
  (setq savehist-autosave-interval 3000))

(after! recentf
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$"
          (lambda (file)
            (file-in-directory-p file package-user-dir)))))

(after! ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(after! bookmark
  (bookmark-maybe-load-default-file)

  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (ymacs-default//bookmark-setup))))

(after! ffap
  ;; do not use ping, it's very slow
  (setq ffap-machine-p-known 'reject))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (setq vc-ignore-dir-regexp
        (format "\\(?:%s\\)\\|\\(?:%s\\)"
                locate-dominating-stop-dir-regexp
                tramp-file-name-regexp))

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

(after! view
  (define-key! :map view-mode-map
    ("s" . ymacs-editor/swiper)
    ("q" . View-exit)
    ("Q" . View-quit)))

(after! xref
  (define-key! :map xref--xref-buffer-mode-map
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("j" . (interactive! () (xref--search-property 'xref-item)))
    ("k" . (interactive! () (xref--search-property 'xref-item t))))

  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(after! grep
  (setq grep-highlight-matches t)
  (setq grep-scroll-output t))

(after! flymake
  (define-key! :map flymake-mode-map
    ("C-c f l" . flymake-show-diagnostics-buffer))

  (define-key! :map flymake-diagnostics-buffer-mode-map
    ("n" . next-line)
    ("j" . next-line)
    ("p" . previous-line)
    ("k" . previous-line)))

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
                  auto-composition-mode)))
  (setq so-long-predicate #'ymacs-default//buffer-has-long-lines-p))

(after! prolog
  (setq prolog-system 'swi))

(after! erc-track
  (setq erc-track-enable-keybindings nil)) ;; erc

(after! pulse
  (setq pulse-delay 0.01))

(after! inf-lisp
  (setq inferior-lisp-program "sbcl"))

(after! image-mode
  (setq image-animate-loop t))

(setq winner-dont-bind-my-keys t)
