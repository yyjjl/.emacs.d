(require 'i3wm-lib)

(defvar i3-document-regexp
  (concat (regexp-opt '(".pdf" ".djvu" ".ps" ".dvi")) "\\'"))

(defvar i3-document-directories '("~/documents/books"
                                  "~/documents/paper"
                                  "~/working/"
                                  "~/project/org/src/"
                                  "~/desktop"))

(defun i3//index-document-files ()
  (let ((case-fold-search t))
    (dolist (directory i3-document-directories)
      (dolist (file (directory-files-recursively directory
                                                 i3-document-regexp))
        (let ((m (recentf-string-member file recentf-list)))
          (unless m
            (push file recentf-list))))))
  (recentf-save-list)
  (message "Indexing done"))

(defun i3/open-document ()
  (interactive)
  (ivy-read "Open: " (mapcar #'substring-no-properties recentf-list)
            :action (lambda (file)
                      (with-ivy-window
                        (recentf-push file)
                        (call-process "gtk-launch" nil nil nil
                                      "evince.desktop"
                                      file)))
            :caller 'i3/open-document))

(defun i3//open-document-transformer ($file)
  "Transform STR to more readable format."
  (concat
   (propertize (file-name-nondirectory $file)
               'face font-lock-string-face)
   " "
   (propertize (abbreviate-file-name (file-name-directory $file))
               'face font-lock-doc-face)))



(with-eval-after-load 'ivy
  (ivy-set-display-transformer 'i3/open-document
                               #'i3//open-document-transformer))

(provide 'i3wm-doc)
