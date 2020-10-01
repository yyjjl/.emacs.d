;; -*- lexical-binding: t; -*-

(defvar ymacs-autosave-interval 300)
(defvar ymacs-autosave-hook nil)

(defvar ymacs-after-init-idle-hook nil)

(defvar ymacs-external-file-extensions
  '("pdf" "djvu" "dvi" "od[fgpst]" "docx?" "xlsx?"
    "pptx?" "mkv" "avi" "mp4" "rmvb"))

(defvar ymacs-external-file-regexp
  (eval-when-compile
    (concat "\\.\\(?:"
            (string-join
             (append (mapcar #'upcase ymacs-external-file-extensions)
                     ymacs-external-file-extensions)
             "\\|")
            "\\)\\'")))

(defvar ymacs-ignored-directories '("auto" "target" "node_modules"
                                    "bower_components" ".sass_cache" ".cache"
                                    ".git" ".cvs" ".svn" ".hg" "elpa"))

(autoload 'ansi-color-apply-on-region "ansi-color")

(unless (file-exists-p ymacs-var-direcotry)
  (make-directory ymacs-var-direcotry))

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
  (setq features-file (expand-file-name "features.el" user-emacs-directory))
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