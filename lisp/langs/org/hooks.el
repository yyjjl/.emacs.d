;;; -*- lexical-binding: t; -*-

(after! ox-publish
  (advice-add #'org-publish-file :around #'without-user-record!!)
  (advice-add #'org-export-dispatch :around #'without-user-record!!))

(after! ob
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))

(after! org
  (put #'org-archive-subtree 'disabled t)

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

      (ymacs-editor/insert-space-around-chinese begin end)))

  (define-advice org-beginning-of-line (:around (-fn &optional -n) smart)
    (if (bolp)
        (back-to-indentation)
      (funcall -fn -n)))

  (define-hook! ymacs-org//setup (org-mode-hook)
    (when buffer-file-name
      (setq-local org-download-image-dir
                  (expand-file-name (format "assets/%s" (file-name-base (buffer-file-name)))))
      (setq-local org-preview-latex-image-directory (expand-file-name "auto/cache/")))

    (auto-fill-mode -1)
    (eldoc-mode -1)

    (when (fboundp 'org-num-mode)
      (org-num-mode 1))

    ;; (add-to-list 'company-backends 'ymacs-org/company-symbols)
    )

  (add-hook 'org-speed-command-hook #'ymacs-org/block-speed-command-activate))

(after! ox-html
  (advice-add #'org-html-publish-to-html :around #'ymacs-org@wrap-publish-fn)

  (define-advice org-html--svg-image (:around (-fn -source -attributes -info) fix)
    (unless (plist-get -attributes :fallback)
      (setq -attributes (plist-put -attributes :fallback -source)))
    (funcall -fn -source -attributes -info)))

(after! ox-latex
  (advice-add #'org-latex-publish-to-pdf :around #'ymacs-org@wrap-publish-fn)

  (define-advice org-latex--label (:around (-fn -datum -info &optional -force -full) maybe-ignore-label)
    (let ((label (funcall -fn -datum -info -force -full)))
      (with-current-buffer (or (find-buffer-visiting (buffer-file-name))
                               (current-buffer))
        (if (and ymacs-org-latex-ignore-internal-label-p
                 (stringp label)
                 (string-prefix-p "\\label{sec:org" label))
            ""
          label))))

  (define-advice org-latex-compile (:after (-texfile &optional _) auto-detele)
    (when ymacs-org-remove-texfile
      (delete-file -texfile)))

  (define-advice org-latex-link (:around (-fn -link -desc -info) handle-pdf)
    "When exporting to pdf, a link to .org file will be change to corresponding .pdf file."
    (let* ((type (org-element-property :type -link))
           (raw-path (org-element-property :path -link))
           ;; (search-option (org-element-property :search-option -link))
           (project (ignore-errors
                      (org-publish-get-project-from-filename
                       (buffer-file-name)))))
      (if (and project
               (string= type "file")
               (string= (file-name-extension raw-path) "org")
               (equal (car (org-publish-get-project-from-filename (file-truename raw-path)))
                      (car project)))
          ;; Search options is not used by PDF
          (format "\\href{%s.pdf}{%s}"
                  (org-latex--protect-text
                   (org-export-file-uri
                    (file-name-sans-extension raw-path)))
                  ;; search-option
                  (or -desc ""))
        (funcall -fn -link -desc -info)))))
