;;; -*- lexical-binding: t; -*-

;; Do not load extra backends
(setq org-export-backends '(html latex beamer))

(after! ob
  (define-key! :map org-babel-map
    ("/" . ymacs-org/split-src-block)))

(after! org
  (require 'company-auctex nil t)

  (define-key! :map org-mode-map
    ("<" . ymacs-org/hot-expand)
    ("C-c t" :map ymacs-org-table-extra-map)
    ("C-c [" . org-reftex-citation)
    ("C-c {" . org-reftex-citation)
    ("C-c C-x [" . org-agenda-file-to-front)
    ("C-c v" . ymacs-org/open-pdf)
    ("M-," . org-mark-ring-goto)
    ("M-." . org-mark-ring-push)
    ("M-n" . ymacs-org/next-item)
    ("M-p" . ymacs-org/previous-item)
    ("C-c l" . org-store-link)
    ("C-M-i" . completion-at-point)
    ("C-c n" . org-next-block)
    ("C-c p" . org-previous-block)
    ("C-c DEL" . org-cycle-list-bullet)
    ([f5] . ymacs-org/open-pdf)
    ([C-f9] . ymacs-org/create-latex-fragemnt-cache)
    ([f9] . ymacs-org/publish-current-file)
    ([f10] . org-publish)
    ("C-c C-t" . org-todo))

  ;; Highlight `{{{color(<color>, <text>}}}' form
  (font-lock-add-keywords
   'org-mode
   '(("{{{[ \n]*\\(bg\\)?color[ \n]*(\\([#0-9a-zA-Z]+\\)[ \n]*,\\([^,]+?\\))}}}"
      (2 (let ((table-p (save-excursion
                          (forward-line 0)
                          (looking-at-p org-table-dataline-regexp))))
           (put-text-property (match-beginning 2) (match-end 2)
                              'display
                              (nth (if table-p 3 1) org-script-display))
           font-lock-builtin-face)
         t t)
      (3 (let ((bg (match-string 1))
               (color (match-string 2)))
           (add-text-properties (match-beginning 0) (match-beginning 2)
                                '(invisible org-link))
           (add-text-properties (match-end 3) (match-end 0)
                                '(invisible org-link))
           (add-text-properties (- (match-beginning 3) 1) (match-beginning 3)
                                '(invisible org-link))
           (list (if (equal bg "bg") :background :foreground) color))
         prepend t))
     ("\\\\\\\\$" . font-lock-comment-face))
   'append)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (dot . t)
     (js . t)
     (perl . t)
     (latex . t)
     (haskell . t)))

  (setq org-entities-user
        (eval-when-compile
          (when (require 'tex-mode nil :noerror)
            (let ((entities (make-hash-table :test #'equal)))
              (dolist (e org-entities)
                (when (consp e)
                  (puthash (car e) t entities)))
              (cl-loop
               for (latex-name . char) in tex--prettify-symbols-alist
               for name = (substring latex-name 1)
               unless (or (not (numberp char)) (gethash name entities))
               collect (list name
                             (concat "\\" name)
                             t
                             (concat "&#" (number-to-string char) ";")
                             name
                             name
                             (char-to-string char)))))))

  (setq org-mouse-1-follows-link nil)

  (setq org-edit-src-content-indentation 0)
  (setq org-edit-timestamp-down-means-later t)

  (setq org-use-speed-commands t)
  (setq org-use-sub-superscripts nil)

  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-o t)

  (setq org-catch-invisible-edits 'smart)
  ;; Number of empty lines needed to keep an empty line between collapsed
  ;; trees.
  (setq org-cycle-separator-lines 2)

  (setq org-ellipsis "...#")
  (setq org-log-done 'time)

  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-span 14)
  (setq org-agenda-include-diary t)
  (setq org-agenda-window-setup 'current-window)

  (setq org-export-with-sub-superscripts t)

  (setq org-adapt-indentation nil)

  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-hide-block-startup nil)

  (setq org-startup-indented nil)
  (setq org-startup-folded nil)

  (setq org-fontify-quote-and-verse-blocks nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)

  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)

  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related nil)
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

  (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)
  (setcdr (assoc 'file org-link-frame-setup) 'find-file)

  ;; Refile targets include this file and any file contributing to the
  ;; agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path 'file)
  ;; Targets complete in steps so we start with filename, TAB shows
  ;; the next level of targets etc
  (setq org-outline-path-complete-in-steps t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(S)"
                    "PROJECT(P@)" "|" "CANCELLED(c@/!)")))
  (setq org-imenu-depth 9)

  (setq org-capture-templates
        `(("t" "Main task" entry
           (file+headline ,(expand-var! "org/*task*") "Task")
           "\n* TODO %^{Task}\n  Captured: %t\n  %?")
          ("i" "Idea" entry
           (file+headline ,(expand-var! "org/*note*") "Idea")
           "\n* %^{Idea}\n  Captured: %U\n  %?")
          ("l" "Link" entry
           (file+headline ,(expand-var! "org/*note*") "Link")
           "\n* %^{Link}\n  Captured: %U\n  %?")
          ("L" "Long term tasks" checkitem
           (file+function ,(expand-var! "org/*task*") ymacs-org//generate-long-term-task)
           "   - [ ] %^{Task} %?"))))

(after! ox-html
  (setq org-html-head
        (eval-when-compile
          (cl-loop
           for (file type) in
           `(("readtheorg/css/htmlize.css" "style")
             (,(expand-etc! "org-manual.css") "style")
             ("lib/js/jquery.min.js" "script")
             ("lib/js/bootstrap.min.js" "script")
             ("lib/js/jquery.stickytableheaders.min.js" "script")
             ("readtheorg/js/readtheorg.js" "script"))
           for directory = (expand-file-name "org-html-themes/styles" ymacs-private-directory)
           for content = (read-file-content! (expand-file-name file directory))
           concat
           (if (string= type "style")
               (concat "<style type=\"text/css\">\n/*<![CDATA[*/\n"
                       content
                       "\n/*]]>*/\n</style>")
             (concat "<script type=\"text/javascript\">"
                     content
                     "</script>")))))
  (setq org-html-mathjax-template
        (eval-when-compile
          (read-file-content!
           (expand-etc! "org-mathjax-template"))))

  (setcdr (assoc 'scale org-html-mathjax-options) '("100"))
  (setcdr (assoc 'align org-html-mathjax-options) '("center")))

(after! ox-latex
  (ignore-errors (require 'ox-bibtex))

  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (add-to-list 'org-latex-minted-langs '(jupyter-python "python"))

  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("latexmk -g -pdf -dvi- -ps- -pdflatex=\"xelatex -shell-escape -interaction=nonstopmode %%O %%S\" -outdir=%o %f"))

  ;; Use minted
  (setq org-latex-listings 'minted)
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-default-class "cn-article")
  (setq org-latex-packages-alist
        '(("" "xeCJK" t)
          ("" "setspace, dcolumn" t)
          ("" "booktabs, wasysym, marvosym" nil)
          ("" "subfig" nil)
          ("" "psfrag, epsfig" t)
          ;; ("OT1" "fontenc" nil)
          ("" "minted" nil)
          ("" "mdframed" t)
          ("" "amsfonts, amsthm, bm, upgreek" t)
          ("mathscr" "eucal" t)
          ("" "geometry" t)
          ("" "tcolorbox" t)
          ("" "verbatim" nil)))
  (let ((common (eval-when-compile
                  (read-file-content!
                   (expand-file-name "common" org-templates-directory)))))
    (add-to-list
     'org-latex-classes
     `("cn-article"
       ,(concat "\\documentclass[11pt,a4paper]{article}\n"
                common)
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list
     'org-latex-classes
     `("cn-book"
       ,(concat "\\documentclass[11pt,openany]{book}\n"
                common)
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")))))

(after! ox-beamer
  (add-to-list
   'org-latex-classes
   `("cn-beamer"
     ,(replace-regexp-in-string
       (regexp-quote "[ORG-TEMPLATE-DIR]")
       org-templates-directory
       (read-file-content!
        (expand-file-name "beamer" org-templates-directory))
       t t)
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! ox-publish
  (setq org-publish-project-alist
        `(("note-pdf"
           :base-directory ,ymacs-org-project-src-dir
           :base-extension "org"
           :publishing-directory ,ymacs-org-project-dst-dir
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
           :html-link-home "../index.html"
           :base-directory ,ymacs-org-project-src-dir
           :base-extension "org"
           :publishing-directory ,ymacs-org-project-dst-dir
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
           :base-directory ,ymacs-org-project-src-dir
           :base-extension
           "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg\\|mp3\\|ogg\\|swf\\|txt\\|asc\\|json"
           :publishing-directory ,ymacs-org-project-dst-dir
           :recursive t
           :publishing-function org-publish-attachment
           :exclude "auto/cache/org-ltximg.*\\.png")
          ("note" :components ("note-pdf" "note-static")))))
