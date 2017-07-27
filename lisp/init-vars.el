(defsubst core|expand-var (name &optional make-p)
  (let ((val (expand-file-name name emacs|var-direcotry)))
    (when (and make-p (not (file-exists-p val)))
      (make-directory val))
    val))
(defsubst core|expand-etc (name)
  (expand-file-name name emacs|etc-direcotry))

(unless (file-exists-p emacs|var-direcotry)
  (make-directory emacs|var-direcotry))

(defvar core|ignored-directories '("auto" "target" "node_modules"
                                   "bower_components" ".sass_cache" ".cache"
                                   ".git" ".cvs" ".svn" ".hg" "elpa"))
;; When buffer's size bigger than `core|large-buffer-size', it will
;; close some features to speed up emacs performance
(defvar core|large-buffer-size (* 1024 1024))

(with-no-warnings
  (setq abbrev-file-name (core|expand-var "abbrev.el"))
  (setq auto-save-list-file-prefix (core|expand-var "auto-save-list/.saves-"))
  (setq backup-directory-alist (list (cons "." (core|expand-var "backups/"))))
  (setq bookmark-default-file (core|expand-var "bookmarks.el"))
  (setq cmake-ide-rdm-executable (core|expand-var "rtags/bin/rdm"))
  (setq cmake-ide-build-pool-dir (core|expand-var "cmake-projects/" t))
  (setq company-statistics-file (core|expand-var
                                 "company-statistics-cache.el"))
  (setq eshell-directory-name (core|expand-var "eshell/"))
  (setq flycheck-eslintrc (core|expand-etc ".eslintrc"))
  (setq irony-server-install-prefix (core|expand-var "irony"))
  (setq irony-user-dir (core|expand-var "irony/"))
  (setq ispell-personal-dictionary (core|expand-var "aspell.pws"))
  (setq mc/list-file (core|expand-var "mc-lists.el"))
  (setq org-publish-timestamp-directory (core|expand-var "org-timestamps/"))
  (setq org-latex-preview-ltxpng-directory (core|expand-var "ltxpng/"))
  (setq org-template-directory (core|expand-etc "org-template"))
  (setq projectile-cache-file (core|expand-var "projectile-cache.el"))
  (setq projectile-known-projects-file (core|expand-var "projectile-bookmarks.el"))
  (setq recentf-save-file (core|expand-var "recentf.el"))
  (setq rtags-path (core|expand-var "rtags/bin"))
  (setq save-place-file (core|expand-var "places.el"))
  (setq savehist-file (core|expand-var "history.el"))
  (setq semanticdb-default-save-directory (core|expand-var "semanticdb/"))
  (setq session-save-file (core|expand-var "session.el"))
  (setq smex-save-file (core|expand-var "smex-items.el"))
  (setq srecode-map-save-file (core|expand-var "srecode-map.el"))
  (setq tramp-persistency-file-name (core|expand-var "tramp.el"))
  (setq url-configuration-directory (core|expand-var "url/")))

(provide 'init-vars)