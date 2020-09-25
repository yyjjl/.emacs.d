;;; -*- lexical-binding: t; -*-

(after! ob
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))

(after! org
  (define-advice org-fill-paragraph (:around (-fn &optional -justify -region) add-space)
    (let* ((element (org-element-context))
           (end (org-element-property :end element))
           (begin (org-element-property :begin element))
           (latex-p (memq (org-element-type element)
                          '(latex-fragment latex-environment))))
      (if (and latex-p (not (region-active-p)) -justify)
          (save-excursion
            (goto-char begin)
            (let ((g1 '(group (not (any "{([" blank))))
                  (g2 '(group (or (any ?= ?- ?+ ?/ ?> ?<)
                                  (and "\\" (or "ne" "neq" "le" "ge"
                                                "cdot" "circ" "cap" "cup"
                                                "subset" "subsetqe" "lor"
                                                "land" "in" "to" "rightarrow"
                                                "Rightarrow")
                                       word-end))))
                  (g3 '(group (not (any ")}]" blank)))))
              (query-replace-regexp (rx-to-string `(and ,g1 ,g2 ,g3) t)
                                    "\\1 \\2 \\3" nil begin end)
              (query-replace-regexp (rx-to-string `(and ,g1 ,g2 (group " ")) t)
                                    "\\1 \\2 " nil begin end)
              (query-replace-regexp (rx-to-string `(and (group " ") ,g2 ,g3) t)
                                    " \\1 \\2" nil begin end)))
        (funcall -fn -justify -region))

      (ymacs-edit/insert-space-around-chinese begin end)))

  (define-advice org-beginning-of-line (:around (-fn &optional -n) smart)
    (if (bolp)
        (back-to-indentation)
      (funcall -fn -n)))

  (define-hook! ymacs-org|setup (org-mode-hook)
    (when buffer-file-name
      (setq-local org-preview-latex-image-directory "auto/cache/"))

    (auto-fill-mode -1)
    (eldoc-mode -1)

    (when (fboundp 'org-num-mode)
      (org-num-mode 1))

    ;; Add context-sensitive completion for meta options
    (setq-local completion-at-point-functions
                (append completion-at-point-functions
                        (list (lambda () (ignore-errors (pcomplete-completions-at-point))))))

    (ymacs-company//add-backend 'ymacs-org/company-symbols :main-backend-p nil))

  (define-hook! ymacs-org|src-setup (org-src-mode-hook)
    (flycheck-mode -1))

  (add-hook 'org-speed-command-hook #'ymacs-org/block-speed-command-activate))

(after! ox-html
  (advice-add #'org-html-publish-to-html :around #'ymacs-org@wrap-publish-fn))

(after! ox-latex
  (advice-add #'org-publish-file :around #'without-user-record!!)
  (advice-add #'org-latex-publish-to-pdf :around #'ymacs-org@wrap-publish-fn)

  (define-advice org-latex--label (:around (-fn -datum -info &optional -force -full) maybe-ignore-label)
    (let ((label (funcall -fn -datum -info -force -full)))
      (with-current-buffer (or (find-buffer-visiting (buffer-file-name))
                               (current-buffer))
        (if (and ymacs-org-latex-ignore-internal-label
                 (stringp label)
                 (string-prefix-p "\\label{sec:org" label))
            ""
          label))))

  (define-advice org-latex-compile (:after (-texfile &optional _) auto-detele)
    (when ymacs-org-remove-texfile
      (delete-file -texfile)))

  (define-advice org-latex-link (:around (-fn -link -desc -info) handle-pdf)
    (let* ((type (org-element-property :type -link))
           (raw-path (org-element-property :path -link))
           (search-option (org-element-property :search-option -link))
           (project (ignore-errors
                      (org-publish-get-project-from-filename
                       (buffer-file-name)))))
      (if (and project
               (string= type "file")
               (string= (file-name-extension raw-path) "org")
               (equal (car (org-publish-get-project-from-filename
                            (file-truename raw-path)))
                      (car project)))
          (format "\\href{%s.pdf%s}{%s}"
                  (org-latex--protect-text
                   (org-export-file-uri
                    (file-name-sans-extension raw-path)))
                  search-option
                  (or -desc ""))
        (funcall -fn -link -desc -info))))

  (define-advice org-latex--inline-image (:around (-fn -link -info) handle-svg)
    (let ((code (funcall -fn -link -info)))
      (replace-regexp-in-string "\\(\\\\includesvg\\)\\(?:[^{]\\)?*{.*}"
                                "\\\\includegraphics"
                                code
                                nil nil 1))))

(after! ox
  (define-advice org-export-expand-include-keyword
      (:around
       (-fn &optional -included -directory &rest -args)
       remove-relative-path)
    "Remove relative file path when including files"

    (if (not -included) ;; function called at top level
        (clrhash ymacs-org--included-files)
      (puthash (file-truename (caar -included)) t ymacs-org--included-files))

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
                                           ymacs-org--included-files)))
                        (goto-char link-end)
                      (let* ((contents-begin (org-element-property :contents-begin link))
                             (contents-end (org-element-property :contents-end link))
                             (description (when (and contents-begin contents-end)
                                            (buffer-substring-no-properties contents-begin contents-end))))
                        (delete-region (org-element-property :begin link)
                                       (org-element-property :end link))
                        (insert (org-link-make-string search-option description) " "))))))))))
      result)))
