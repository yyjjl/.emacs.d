(defvar emacs-gc-cons-threshold (* 100 1024 1024))

(defvar emacs-file-name-handler-alist file-name-handler-alist
  "Save `file-name-handler-alist' temporarily and set it to nil
which means on every .el and .elc file loaded during start up, it
hasn't to runs those regexps against the filename.")

(defvar emacs-private-directory
  (expand-file-name "private" user-emacs-directory)
  "Local packages in this directory")

(defvar emacs-etc-direcotry
  (expand-file-name "etc" user-emacs-directory)
  "Some configuration file in this directory")

(defvar emacs-autoloads-directory
  (expand-file-name "autoloads" emacs-config-directory)
  "Autoloads files in this directory")
(defvar emacs-autoloads-file (expand-var! "autoloads.el")
  "Autoloads file")

(defvar core-ignored-directories '("auto" "target" "node_modules"
                                   "bower_components" ".sass_cache" ".cache"
                                   ".git" ".cvs" ".svn" ".hg" "elpa"))

(defvar core-large-buffer-size (* 1024 1024)
  "When buffer's size bigger than `core-large-buffer-size', it
will close some features to speed up emacs performance")

(defvar after-init-idle-hook nil)

(setvar!
 ;; Use `xsel' to copy/paste in terminal thoungh system clipboard
 env-has-xsel-p (executable-find "xsel")
 ;; Whether to use X input method `fcitx'
 env-has-fcitx-p (executable-find "fcitx")
 ;; In order to use `emms' package, you need a music player
 env-has-mpv-p (executable-find "mplayer")
 env-has-ripgrep-p (executable-find "rg")
 ;; Whether has `git'
 env-has-git-p (executable-find "git")
 ;; Only one of `aspell' and `hunspell' is needed Use for spellcheck
 env-has-aspell-p (executable-find "aspell")
 ;; Use for spellcheck
 env-has-hunspell-p (executable-find "hunspell")
 ;; Tag multiple languages
 env-has-gtags-p (executable-find "global")
 ;; Whether to use latex packages
 env-has-latex-p (executable-find "xelatex"))

(unless (file-exists-p emacs-var-direcotry)
  (make-directory emacs-var-direcotry))

(with-no-warnings
  (setq TeX-auto-global (expand-var! "auctex/"))
  (setq abbrev-file-name (expand-var! "abbrev.el"))
  (setq auto-save-list-file-prefix (expand-var! "auto-save-list/"))
  (setq backup-directory-alist (list (cons "." (expand-var! "backups/"))))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))
  (setq bookmark-default-file (expand-var! "bookmarks.el"))
  (setq calc-settings-file (expand-var! "calc.el"))
  (setq cnfonts-directory (expand-var! "cnfonts/"))
  (setq cmake-ide-rdm-executable (expand-var! "rtags/bin/rdm"))
  (setq company-statistics-file (expand-var! "company-statistics-cache.el"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq dap-breakpoints-file (expand-var! ".dap-breakpoints"))
  (setq desktop-dirname (expand-var! "desktop"))
  (setq desktop-path (list desktop-dirname))
  (setq desktop-restore-frames nil)
  (setq emms-directory (expand-var! "emms"))
  (setq eshell-directory-name (expand-var! "eshell/"))
  (setq gnus-directory (expand-var! "gnus"))
  (setq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory))
  (setq irony-server-install-prefix (expand-var! "irony"))
  (setq irony-user-dir (expand-var! "irony/"))
  (setq ispell-personal-dictionary (expand-var! "aspell.pws"))
  (setq mc/list-file (expand-var! "mc-lists.el"))
  (setq org-preview-latex-image-directory (expand-var! "ltximg/"))
  (setq org-publish-timestamp-directory (expand-var! "org-timestamps/"))
  (setq org-default-notes-file (expand-var! "org/*note*"))
  (setq org-export-async-init-file (expand-etc! "org-async-init.el"))
  (setq org-templates-directory (expand-etc! "org-templates"))
  (setq projectile-cache-file (expand-var! "projectile-cache.el"))
  (setq projectile-known-projects-file (expand-var! "projectile-bookmarks.el"))
  (setq pyim-directory (expand-var! "pyim"))
  (setq pyim-dcache-directory (expand-var! "pyim/dcache"))
  (setq recentf-save-file (expand-var! "recentf.el"))
  (setq request-storage-directory (expand-var! "request"))
  (setq rtags-path (expand-var! "rtags/bin"))
  (setq save-place-file (expand-var! "places.el"))
  (setq savehist-file (expand-var! "history.el"))
  (setq semanticdb-default-save-directory (expand-var! "semanticdb/"))
  (setq session-save-file (expand-var! "session.el"))
  (setq skeletor-project-directory (expand-file-name "~/working/"))
  (setq skeletor-user-directory (expand-etc! "project-templates/"))
  (setq smex-save-file (expand-var! "smex-items.el"))
  (setq amx-save-file (expand-var! "amx-items.el"))
  (setq srecode-map-save-file (expand-var! "srecode-map.el"))
  (setq transient-history-file (expand-var! "transient/history.el"))
  (setq tramp-persistency-file-name (expand-var! "tramp.el"))
  (setq treemacs-persist-file (expand-var! "treemacs-persist"))
  (setq url-configuration-directory (expand-var! "url/")))

(add-auto-mode! 'conf-mode
  "\\.[^b][^a][a-zA-Z]*rc$"
  "\\.aspell\\.en\\.pws\\'"
  "\\.meta\\'"
  "\\.?muttrc\\'"
  "\\.ctags\\'"
  "\\.mailcap\\'")

(add-to-list 'interpreter-mode-alist '("node" . js-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(provide 'core-vars)
