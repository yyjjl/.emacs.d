;;; -*- lexical-binding: t; -*-

;; Config for publish site from org files
(defvar org-project-base-dir "~/project/org")
(defvar org-project-src-dir "~/project/org/src")

(defcustom org-project-sync-command nil
  "Command to sync org notes."
  :group 'org
  :type 'directory
  :safe #'stringp)

(defcustom org-master-file nil
  "master file for org"
  :group 'org
  :type 'file
  :safe #'stringp)


(defvar org--included-files (make-hash-table :test #'equal))
(defun org*around-export-expand-include-keyword (-fn &optional -included -directory &rest -args)
  "Remove relative file path when including files"
  (if (not -included) ;; function called at top level
      (clrhash org--included-files)
    (puthash (file-truename (caar -included)) t org--included-files))

  (let ((result (apply -fn -included -directory -args)))
    (unless -included
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-link-any-re nil t)
          (let ((link-end (point))
                (link (save-excursion
                        (forward-char -1)
                        (save-match-data (org-element-context)))))
            (when (eq 'link (org-element-type link))
              (let ((link-begin (org-element-property :begin link)))
                (goto-char link-begin)
                (let* ((path (org-element-property :path link))
                       (search-option (org-element-property :search-option link)))
                  (if (not (and search-option
                                (eq 'bracket (org-element-property :format link))
                                (string= "file" (org-element-property :type link))
                                (gethash (file-truename (expand-file-name path -directory))
                                         org--included-files)))
                      (goto-char link-end)
                    (let* ((contents-begin (org-element-property :contents-begin link))
                           (contents-end (org-element-property :contents-end link))
                           (description (when (and contents-begin contents-end)
                                          (buffer-substring-no-properties contents-begin contents-end))))
                      (delete-region (org-element-property :begin link)
                                     (org-element-property :end link))
                      (insert (org-link-make-string search-option description) " "))))))))))
    result))

(defun org*wrap-publish-fn (-fn -plist -filename -pub-dir)
  (condition-case err
      (funcall -fn -plist -filename -pub-dir)
    (error (message "Fail to publish file %s: %s" -filename err))))

(with-eval-after-load 'ox
  (advice-add 'org-export-expand-include-keyword :around #'org*around-export-expand-include-keyword))

(with-eval-after-load 'ox-html
  (advice-add 'org-html-publish-to-html :around #'org*wrap-publish-fn))

(with-eval-after-load 'ox-latex
  (advice-add 'org-latex-publish-to-pdf :around #'org*wrap-publish-fn))

(with-eval-after-load 'ox-publish
  (setq org-publish-project-alist
        `(("note-pdf"                   ; These are the main web files
           :base-directory ,org-project-src-dir
           :base-extension "org"
           :publishing-directory ,org-project-src-dir
           :recursive t
           :publishing-function org-latex-publish-to-pdf
           :headline-levels 4           ; Just the default for this project.
           :auto-preamble nil
           :section-numbers t
           :table-of-contents t
           :with-sub-superscript nil
           :auto-sitemap t
           :sitemap-filename "index.org")
          ("note-html"                  ; These are the main web files
           :base-directory ,org-project-src-dir
           :base-extension "org"
           :publishing-directory ,org-project-src-dir
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4           ; Just the default for this project.
           :auto-preamble nil
           :section-numbers t
           :table-of-contents t
           :with-sub-superscript nil
           :auto-sitemap t
           :sitemap-filename "index.org")
          ("note-static"            ; These are static files (images, pdf, etc)
           :base-directory ,org-project-base-dir
           :base-extension
           "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg\\|mp3\\|ogg\\|swf\\|txt\\|asc\\|json"
           :publishing-directory ,org-project-base-dir
           :recursive t
           :publishing-function org-publish-attachment)
          ("note" :components ("note-pdf" "note-static")))))

(global-set-key (kbd "C-x O") 'org/project-open)

(provide 'init-notes)
