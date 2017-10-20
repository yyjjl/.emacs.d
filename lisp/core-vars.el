(defvar emacs-gc-cons-threshold (* 100 1024 1024))

(defvar emacs-file-name-handler-alist file-name-handler-alist
  "Save `file-name-handler-alist' temporarily and set it to nil
which means on every .el and .elc file loaded during start up, it
hasn't to runs those regexps against the filename.")

(defvar emacs-site-packages-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "Local packages in this directory")

(defvar emacs-etc-direcotry
  (expand-file-name "etc" user-emacs-directory)
  "Some configuration file in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "var" user-emacs-directory)
  "All data and external executable file in this direcotry")

(defvar core-ignored-directories '("auto" "target" "node_modules"
                                   "bower_components" ".sass_cache" ".cache"
                                   ".git" ".cvs" ".svn" ".hg" "elpa"))

(defvar core-large-buffer-size (* 1024 1024)
  "When buffer's size bigger than `core-large-buffer-size', it
will close some features to speed up emacs performance")

(unless (file-exists-p emacs-var-direcotry)
  (make-directory emacs-var-direcotry))

(with-no-warnings
  (setq TeX-auto-global (expand-var! "auctex/"))
  (setq abbrev-file-name (expand-var! "abbrev.el"))
  (setq auto-save-list-file-prefix (expand-var! "auto-save-list/.saves-"))
  (setq backup-directory-alist (list (cons "." (expand-var! "backups/"))))
  (setq bookmark-default-file (expand-var! "bookmarks.el"))
  (setq calc-settings-file (expand-var! "calc.el"))
  (setq cmake-ide-build-pool-dir (expand-var! "cmake-projects/" t))
  (setq cmake-ide-rdm-executable (expand-var! "rtags/bin/rdm"))
  (setq company-statistics-file (expand-var! "company-statistics-cache.el"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq emms-directory (expand-var! "emms"))
  (setq eshell-directory-name (expand-var! "eshell/"))
  (setq gnus-init-file (expand-var! ".gnus"))
  (setq irony-server-install-prefix (expand-var! "irony"))
  (setq irony-user-dir (expand-var! "irony/"))
  (setq ispell-personal-dictionary (expand-var! "aspell.pws"))
  (setq mc/list-file (expand-var! "mc-lists.el"))
  (setq org-latex-preview-ltxpng-directory (expand-tmp! "ltxpng/"))
  (setq org-publish-timestamp-directory (expand-var! "org-timestamps/"))
  (setq org-template-directory (expand-etc! "org-template"))
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
  (setq srecode-map-save-file (expand-var! "srecode-map.el"))
  (setq tramp-persistency-file-name (expand-var! "tramp.el"))
  (setq url-configuration-directory (expand-var! "url/")))

;; Set envrionment variables
(require 'core-env-vars)

(add-auto-mode! 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws\\'"
               "\\.meta\\'"
               "\\.?muttrc\\'"
               "\\.ctags\\'"
               "\\.mailcap\\'")

(add-auto-mode! 'groovy-mode
               "\\.groovy\\'"
               "\\.gradle\\'")

(add-auto-mode! 'crontab-mode
               "\\.?cron\\(tab\\)?\\'")

;; cmake
(add-auto-mode! 'cmake-mode
               "CMakeLists\\.txt\\'"
               "\\.cmake\\'")
;; markdown
(add-auto-mode! 'markdown-mode
               "\\.\\(md\\|markdown\\)\\'")

(add-auto-mode! 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'")

(add-auto-mode! 'js-mode "\\.json\\'" "\\.jason\\'" "\\.jshintrc\\'")
(add-auto-mode! 'js2-mode "\\.js\\(\\.erb\\)?\\'")
(add-auto-mode! 'js2-jsx-mode "\\.jsx?\\'")

(add-auto-mode! 'sh-mode
               "\\.basj_profile\\'" "\\.bash_history\\'"
               "\\.sh\\'" "\\.bash\\'" "\\.bashrc.local\\'"
               "\\.zsh\\'" "\\.bashrc\\'" "\\.zshrc\\'")

(add-auto-mode! 'web-mode
               "\\.phtml\\'" "\\.cmp\\'" "\\.app\\'"
               "\\.page\\'" "\\.component\\'"
               "\\.wp\\'" "\\.tmpl\\'" "\\.php\\'"
               "\\.module\\'" "\\.inc\\'" "\\.hbs\\'"
               "\\.tpl\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
               "\\.erb\\'" "\\.mustache\\'"
               "\\.djhtml\\'" "\\.ftl\\'"
               "\\.html?\\'" "\\.xul?\\'" "\\.eex?\\'"
               "\\.xml?\\'")

(add-auto-mode! 'glsl-mode
               "\\.glsl\\'" "\\.vert\\'"
               "\\.frag\\'" "\\.geom\\'")

(add-auto-mode! 'latex-mode "\\.tikz\\'")

(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("python" .   python-mode))

(provide 'core-vars)
