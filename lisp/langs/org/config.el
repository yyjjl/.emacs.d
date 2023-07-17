;;; -*- lexical-binding: t; -*-

;; Do not load extra backends
(setq org-export-backends '(html latex beamer md))

(after! ob
  (define-key! :map org-babel-map
    ("/" . ymacs-org/split-src-block)))

(after! org
  (require 'latex nil t)

  (ymacs-editor//add-toggles
   "Org"
   '(eq major-mode 'org-mode)
   '("w" ymacs-org/latexmk-start-watching "Start LaTeXMK watching" :exit t)
   '("d" ymacs-org/delete-latex-fragement-cache "Delete LaTeX Preview" :exit t))

  (define-key! :map org-mode-map
    ("<" . ymacs-org/hot-expand)
    ("C-c t" :map ymacs-org-table-extra-map)
    ("C-c [" . org-reftex-citation)
    ("M-," . org-mark-ring-goto)
    ("M-." . org-mark-ring-push)
    ("M-n" . ymacs-org/next-item)
    ("M-p" . ymacs-org/previous-item)
    ("C-c b" . org-switchb)
    ("C-c C-b" . org-indent-line)
    ("C-c p" . org-previous-visible-heading)
    ("C-c n" . org-next-visible-heading)
    ("C-c DEL" . org-cycle-list-bullet)
    ("C-x , l" . org-latex-preview)
    ("C-x , ." . org-store-link)
    ("C-x , i" . org-toggle-inline-images)
    ("C-x , [" . org-next-block)
    ("C-x , ]" . org-previous-block)
    ("C-c d d" . org-download-screenshot)
    ("C-c d r" . org-download-rename-at-point)
    ("C-c d l" . org-download-rename-last-file)
    ([f5] . ymacs-org/open)
    ([f9] . ymacs-org/publish-current-file)
    ([f10] . org-publish)
    ([f12] . ymacs-hydra/org-download/body))

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
        '(("flat" "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯")))

  (setq org-mouse-1-follows-link nil)

  (setq org-edit-src-content-indentation 0)
  (setq org-edit-timestamp-down-means-later t)

  (setq org-use-speed-commands t)
  (setq org-use-sub-superscripts nil)

  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-o t)

  (setq org-fold-catch-invisible-edits 'smart)
  ;; Number of empty lines needed to keep an empty line between collapsed
  ;; trees.
  (setq org-cycle-separator-lines 2)

  (setq org-ellipsis "...#")
  (setq org-log-done 'time)

  (setq org-image-actual-width t)
  (setq org-imenu-depth 8)

  (setq org-export-with-sub-superscripts t)

  (setq org-adapt-indentation nil)

  (setq org-hide-emphasis-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-hide-block-startup nil)

  (setq org-startup-indented nil)
  (setq org-startup-folded 'content)

  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-whole-block-delimiter-line t)

  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)

  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-src-window-setup 'other-window)

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
                    "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

(after! ox-html
  (setq org-html-style-default
        (eval-when-compile
          (require 'ox-html)
          (if (string-match
               (rx ".org-svg" (+ (not "{")) (+ (not digit)) (group (+ digit) "%"))
               org-html-style-default)
              (replace-match "auto" nil nil org-html-style-default 1)
            org-html-style-default)))

  (setq org-html-head
        (eval-when-compile
          (concat "<style type=\"text/css\">\n/*<![CDATA[*/\n"
                  (with-demoted-errors "Error %s"
                    (with-temp-buffer
                      (erase-buffer)
                      (url-insert-file-contents "https://gongzhitaao.org/orgcss/org.css")
                      (buffer-string)))
                  (read-file-content! (expand-etc! "org-common.css"))
                  "\n/*]]>*/\n</style>")))
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
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-default-class "cn-article")
  (setq org-latex-packages-alist
        '(("" "xeCJK" t)
          ("" "svg" nil)
          ("" "setspace, dcolumn" t)
          ("" "booktabs, wasysym, marvosym" nil)
          ("" "subfig" nil)
          ("" "psfrag, epsfig" t)
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
       ,(concat "\\documentclass[11pt,a4paper]{article}\n" common)
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list
     'org-latex-classes
     `("cn-book"
       ,(concat "\\documentclass[11pt,openany]{book}\n" common)
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

(after! org-download
  (setq org-download-heading-lvl nil))

(after! ox-publish
  (setq
   org-publish-project-alist
   `(("note-pdf"
      :base-directory ,ymacs-org-project-src-dir
      :base-extension "org"
      :publishing-directory ,ymacs-org-project-dst-dir
      :recursive t
      :publishing-function org-latex-publish-to-pdf
      :headline-levels 4                ; Just the default for this project.
      :auto-preamble nil
      :section-numbers t
      :table-of-contents t
      :preserve-breaks t
      :with-sub-superscript nil
      :auto-sitemap nil
      :sitemap-filename "index.org")
     ("note-html"                       ; These are the main web files
      :html-link-home "../index.html"
      :base-directory ,ymacs-org-project-src-dir
      :base-extension "org"
      :publishing-directory ,ymacs-org-project-dst-dir
      :recursive t
      :publishing-function org-html-publish-to-html
      :headline-levels 4                ; Just the default for this project.
      :auto-preamble nil
      :section-numbers t
      :table-of-contents t
      :preserve-breaks t
      :with-date t
      :with-sub-superscript nil
      :auto-sitemap t
      :sitemap-title "Notes"
      :sitemap-filename "index.org")
     ("note-static"                 ; These are static files (images, pdf, etc)
      :base-directory ,ymacs-org-project-src-dir
      :base-extension
      "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg\\|mp3\\|ogg\\|swf\\|txt\\|asc\\|json"
      :publishing-directory ,ymacs-org-project-dst-dir
      :recursive t
      :publishing-function org-publish-attachment
      :exclude "auto/cache/org-ltximg.*\\.png")
     ("note" :components ("note-pdf" "note-static")))))
