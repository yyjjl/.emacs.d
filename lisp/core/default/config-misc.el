;;; -*- lexical-binding: t; -*-

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
    ("j"  (defun ymacs-default/xref-next ()
             (interactive)
             (xref--search-property 'xref-item)))
    ("k" (defun ymacs-default/xref-prev ()
           (interactive)
           (xref--search-property 'xref-item t))))

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
    ("k" . previous-line))

  (setq-default flymake-diagnostic-functions nil))

(after! info
  (setq info-lookup-other-window-flag nil))

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
(after! winner
  (setq winner-boring-buffers-regexp "^ \\*"))

