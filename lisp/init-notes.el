;;; -*- lexical-binding: t; -*-

;; Config for publish site from org files
(defvar org-project-base-dir "~/project/org")
(defvar org-project-src-dir "~/project/org/src")

(defcustom org-project-sync-command nil
  "Command to sync org notes."
  :group 'org
  :type 'directory
  :safe #'stringp)

(setq org-publish-project-alist
      `(("note-pdf"                     ; These are the main web files
         :base-directory ,org-project-src-dir
         :base-extension "org"
         :publishing-directory ,org-project-src-dir
         :recursive t
         :publishing-function org-latex-publish-to-pdf
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble nil
         :section-numbers t
         :table-of-contents t
         :with-sub-superscript nil
         :auto-sitemap t
         :sitemap-filename "index.org")
        ("note-static"              ; These are static files (images, pdf, etc)
         :base-directory ,org-project-base-dir
         :base-extension
         "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg\\|mp3\\|ogg\\|swf\\|txt\\|asc\\|json"
         :publishing-directory ,org-project-base-dir
         :recursive t
         :publishing-function org-publish-attachment)
        ("note" :components ("note-pdf" "note-static"))))

(defun org/project-open ()
  (interactive)
  (let* ((prefix-length (length (file-name-as-directory
                                 (file-truename org-project-src-dir))))
         (name (ivy-read "Open note: "
                         (--map
                          (substring it prefix-length)
                          (directory-files-recursively org-project-src-dir
                                                       "\\.org\\'"))
                         :history 'org-project-note-history
                         :caller 'org/project-open)))
    (setq name (expand-file-name name org-project-src-dir))
    (if (string-suffix-p ".org" name)
        (and (or (file-exists-p name)
                 (y-or-n-p (format "Create new note %s"
                                   (abbreviate-file-name name))))
             (find-file name))
      (message "Note must be an org file"))))

(defun org/project-sync ()
  (interactive)
  (unless org-project-sync-command
    (error "Variable `org-project-sync-command' is not set."))
  (let ((default-directory org-project-base-dir))
    (compilation-start org-project-sync-command)))

(global-set-key (kbd "C-x O") 'org/project-open)

(provide 'init-notes)