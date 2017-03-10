(add-auto-mode 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws\\'"
               "\\.meta\\'"
               "\\.?muttrc\\'"
               "\\.ctags\\'"
               "\\.mailcap\\'")

(add-auto-mode 'groovy-mode
               "\\.groovy\\'"
               "\\.gradle\\'")

(add-auto-mode 'crontab-mode
               "crontab.*\\'"
               "\\.?cron\\(tab\\)?\\'")

;; cmake
(add-auto-mode 'cmake-mode
               "CMakeLists\\.txt\\'"
               "\\.cmake\\'")
;; markdown
(add-auto-mode 'markdown-mode
               "\\.\\(md\\|markdown\\)\\'")

(add-auto-mode 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'")

(add-auto-mode 'js-mode "\\.json\\'" "\\.jason\\'" "\\.jshintrc\\'")
(add-auto-mode 'js2-mode "\\.js\\(\\.erb\\)?\\'")
(add-auto-mode 'js2-jsx-mode "\\.jsx?\\'")

(add-auto-mode 'sh-mode
               "\\.basj_profile\\'" "\\.bash_history\\'"
               "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
               "\\.zsh\\'" "\\.bashrc\\'")

(add-auto-mode 'web-mode
               "\\.phtml\\'" "\\.cmp\\'" "\\.app\\'"
               "\\.page\\'" "\\.component\\'"
               "\\.wp\\'" "\\.tmpl\\'" "\\.php\\'"
               "\\.module\\'" "\\.inc\\'" "\\.hbs\\'"
               "\\.tpl\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
               "\\.erb\\'" "\\.mustache\\'"
               "\\.djhtml\\'" "\\.ftl\\'"
               "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'"
               "\\.xml?\\'")

(add-auto-mode 'glsl-mode
               "\\.glsl\\'" "\\.vert\\'"
               "\\.frag\\'" "\\.geom\\'")


(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("python" .   python-mode))



(setq-default tab-width 4)
(setq history-delete-duplicates t)
(fset 'yes-or-no-p 'y-or-n-p)
;; no automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)
;; from robinh, time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; (display-time)
;; prolog system
(setq prolog-system 'swi)
;; regex-tool setup
(setq regex-tool-backend 'perl)
(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              save-interprogram-paste-before-kill t
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0.2
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

(setq projectile-completion-system 'ivy)
(setq system-time-locale "C")
(setq imenu-max-item-length 1024)
(setq minibuffer-prompt-properties
      '(read-only
        t
        point-entered
        minibuffer-avoid-prompt
        face
        minibuffer-prompt))

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq csv-separators '("," ";" "|" " "))

;; Write backup files to own directory
(if (not (file-exists-p (expand-file-name "~/.emacs.d/data/backups")))
    (make-directory (expand-file-name "~/.emacs.d/data/backups")))

(setq backup-by-coping t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/data/backups"))
      delete-old-versions t
      version-control t  ;use versioned backups
      kept-new-versions 6
      kept-old-versions 2)
;; Donot make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)

;;Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
;; default prog-mode setup
(defun generic-prog-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; (subword-mode 1)
    ;; auto insert closing pair
    (electric-pair-mode 1)
    ;; eldoc, show API doc in minibuffer echo area
    (eldoc-mode 1)
    (show-paren-mode 1)
    ;; show trailing spaces in a programming mode
    (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)

;; {{ display long lines in truncated style (end line with $)
(defun truncate-lines-setup ()
  (toggle-truncate-lines 1))
(add-hook 'grep-mode-hook 'truncate-lines-setup)
;; }}

(defalias 'perl-mode 'cperl-mode)

(global-set-key [escape] 'keyboard-escape-quit)

(provide 'init-defaults)