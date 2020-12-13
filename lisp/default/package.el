;; -*- lexical-binding: t; -*-

(defvar ymacs-autosave-interval 300)
(defvar ymacs-autosave-hook nil)

(defvar ymacs-after-init-idle-hook nil)

(defvar ymacs-external-file-extensions
  '("pdf" "djvu" "dvi" "od[fgpst]" "docx?" "xlsx?"
    "pptx?" "mkv" "avi" "mp4" "rmvb"))

(defun ymacs//make-external-file-extensions-regexp (-extentions)
  (concat "\\.\\(?:"
          (string-join
           (append (mapcar #'upcase -extentions)
                   -extentions)
           "\\|")
          "\\)\\'"))

(defvar ymacs-external-file-regexp
  (eval-when-compile
    (ymacs//make-external-file-extensions-regexp
     ymacs-external-file-extensions)))

(defvar ymacs-ignored-directories '("auto" "target" "node_modules"
                                    "bower_components" ".sass_cache" ".cache"
                                    ".git" ".cvs" ".svn" ".hg" "elpa"))

(defvar ymacs-auto-next-error-buffer-derived-modes
  '(occur-mode
    grep-mode
    ivy-occur-mode
    xref--xref-buffer-mode
    compilation-mode))

(autoload 'ansi-color-apply-on-region "ansi-color")

(unless (file-exists-p ymacs-cache-direcotry)
  (make-directory ymacs-cache-direcotry))

(with-no-warnings
  (setq TeX-auto-global (expand-cache! "auctex/"))
  (setq abbrev-file-name (expand-cache! "abbrev.el"))
  (setq amx-save-file (expand-cache! "amx-items.el"))
  (setq backup-directory-alist (list (cons "." (expand-cache! "backups/"))))
  (setq bookmark-default-file (expand-cache! "bookmarks.el"))
  (setq calc-settings-file (expand-cache! "calc.el"))
  (setq cnfonts-directory (expand-cache! "cnfonts/"))
  (setq company-statistics-file (expand-cache! "company-statistics-cache.el"))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq dap-breakpoints-file (expand-cache! ".dap-breakpoints"))
  (setq desktop-dirname (expand-cache! "desktop"))
  (setq desktop-path (list desktop-dirname))
  (setq desktop-restore-frames nil)
  (setq emms-directory (expand-cache! "emms"))
  (setq eshell-directory-name (expand-cache! "eshell/"))
  (setq features-file (expand-file-name "features.el" user-emacs-directory))
  (setq gnus-directory (expand-cache! "gnus"))
  (setq gnus-init-file (expand-etc! "gnus.el"))
  (setq irony-server-install-prefix (expand-cache! "irony"))
  (setq irony-user-dir (expand-cache! "irony/"))
  (setq ispell-personal-dictionary (expand-cache! "aspell.pws"))
  (setq mc/list-file (expand-cache! "mc-lists.el"))
  (setq org-default-notes-file (expand-cache! "org/*note*"))
  (setq org-export-async-init-file (expand-etc! "org-async-init.el"))
  (setq org-preview-latex-image-directory (expand-cache! "ltximg/"))
  (setq org-publish-timestamp-directory (expand-cache! "org-timestamps/"))
  (setq org-templates-directory (expand-etc! "org-templates"))
  (setq package-quickstart-file (expand-cache! "quickstart.el"))
  (setq projectile-cache-file (expand-cache! "projectile-cache.el"))
  (setq projectile-known-projects-file (expand-cache! "projectile-bookmarks.el"))
  (setq pyim-dcache-directory (expand-cache! "pyim/dcache"))
  (setq pyim-directory (expand-cache! "pyim"))
  (setq recentf-save-file (expand-cache! "recentf.el"))
  (setq request-storage-directory (expand-cache! "request"))
  (setq rtags-path (expand-cache! "rtags/bin"))
  (setq save-place-file (expand-cache! "places.el"))
  (setq savehist-file (expand-cache! "history.el"))
  (setq semanticdb-default-save-directory (expand-cache! "semanticdb/"))
  (setq session-save-file (expand-cache! "session.el"))
  (setq skeletor-project-directory (expand-file-name "~/working/"))
  (setq skeletor-user-directory (expand-etc! "project-templates/"))
  (setq smex-save-file (expand-cache! "smex-items.el"))
  (setq srecode-map-save-file (expand-cache! "srecode-map.el"))
  (setq tramp-persistency-file-name (expand-cache! "tramp.el"))
  (setq transient-history-file (expand-cache! "transient/history.el"))
  (setq treemacs-persist-file (expand-cache! "treemacs-persist"))
  (setq url-configuration-directory (expand-cache! "url/")))
