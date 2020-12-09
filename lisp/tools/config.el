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

(after! savehist
  (setq savehist-autosave-interval 3000))

(after! recentf
  (defun ymacs-tools//recentf-keep-p (-fn)
    (and ymacs--buffer-visible-p
         ;; The order must be kept
         (or (file-remote-p -fn)
             (and (file-readable-p -fn)
                  (file-writable-p -fn)))))

  (setq recentf-keep '(ymacs-tools//recentf-keep-p))
  (setq recentf-max-saved-items 2048)
  (setq recentf-exclude
        (list "/tmp/" "^/sshx?:" "/sudo:" "\\.elc$"
              "\\.?cache" ".cask"
              "/node_modules/"
              "\\.\\(gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "/TAGS$"
              ymacs-var-direcotry
              (expand-file-name package-user-dir))))

(after! bookmark
  (bookmark-maybe-load-default-file)

  ;; Setup for existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (ymacs-tools|bookmark-setup))))

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

(after! fcitx
  ;; Init fcitx prefix keys
  (setq fcitx-use-dbus nil)
  (fcitx-prefix-keys-add "C-h" "M-g" "M-s" "M-o" "C-x" "C-c" "C-z"))

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

(after! projectile
  (define-key! :map projectile-mode-map
    ("C-x p" :map projectile-command-map))

  (define-key! :map projectile-command-map
    ("E" . ymacs-tools/edit-dir-locals)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project))

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
                  auto-composition-mode)))
  (setq so-long-predicate #'ymacs//buffer-has-long-lines-p))

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

(setq winner-dont-bind-my-keys t)