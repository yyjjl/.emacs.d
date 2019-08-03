;;; -*- lexical-binding: t; -*-

(require-packages!
 ;; (org :compile (org ob ox-html org-table))
 (org-plus-contrib :compile (org ob ox-html org-table))
 ;; Export colorful src block in `org-mode'
 htmlize
 poporg
 company-auctex)



;; Do not load extra backends
(setq org-export-backends '(html latex beamer))

(defvar org--remove-texfile t)
(defvar org-block-key-bindings
  '(("p" . org-previous-block)
    ("\C-p" . org-previous-block)
    ("n" . org-next-block)
    ("\C-n" . org-next-block)))

(defvar org-table-extra-map
  (define-key! :map (make-sparse-keymap)
    ("t" . orgtbl-insert-radio-table)
    ("c" . org-table-create)
    ("I" . org-table-import)
    ("e" . org-table-export)
    ("d" . org-table-delete-column)
    ("i" . org-table-insert-column)
    ("r" . org-table-show-reference)))

(defmacro org/by-backend (&rest -body)
  `(case (or (and (boundp 'org-export-current-backend)
                  org-export-current-backend)
             'babel)
     ,@-body))

(defun org*around-latex-inline-image (-fn -link -info)
  (let ((code (funcall -fn -link -info)))
    (replace-regexp-in-string "\\(\\\\includesvg\\)\\(?:[^{]\\)?*{.*}"
                              "\\\\includegraphics"
                              code
                              nil nil 1)))

(defun org*around-html-do-format-code (-fn &rest -rest)
  (when (= (length -rest) 5)
    (let ((num-start (nth 4 -rest)))
      (unless num-start
        (setq num-start 0))
      (setcar (nthcdr 4 -rest) num-start)))
  (apply -fn -rest))

(defun org*around-latex-link (-fn -link -desc -info)
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

(defun org*around-fill-paragraph (-fn &optional -justify -region)
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
    (when (fboundp #'extra/insert-space-around-chinese)
      (ignore-errors (extra/insert-space-around-chinese begin end)))))

(defun org*after-latex-compile (-texfile &optional _)
  (when org--remove-texfile
    (delete-file -texfile)))

(defun org-block-speed-command-activate (keys)
  "Hook for activating single-letter block commands."
  (when (and (bolp)
             (looking-at
              (eval-when-compile
                (concat
                 "^[\t ]*#\\+begin_?\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?"
                 "\\|"
                 "^[\t ]*#\\+end_?\\([^ \n]+\\)$"))))
    (cdr (assoc keys org-block-key-bindings))))

(with-eval-after-load 'ob
  (define-key! :map org-babel-map
    ("/" . org/split-src-block))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(define-hook! org|setup (org-mode-hook)
  (when buffer-file-name
    (setq-local org-preview-latex-image-directory "auto/cache/"))
  (auto-fill-mode -1)
  (eldoc-mode -1)

  ;; Add context-sensitive completion for meta options
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               (lambda ()
                 (ignore-errors (pcomplete-completions-at-point))))
  (add-to-list 'company-backends 'company-org-symbols))

(define-hook! org|src-setup (org-src-mode-hook)
  (flycheck-mode -1))

(with-eval-after-load 'org
  (require 'company-auctex)

  (add-hook 'org-speed-command-hook 'org-block-speed-command-activate :append)

  (advice-add 'org-fill-paragraph :around #'org*around-fill-paragraph)

  (advice-add 'org-beginning-of-line
              :around (lambda (-fn &optional -n)
                        (if (bolp) (back-to-indentation) (funcall -fn -n))))

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

  (org-babel-do-load-languages 'org-babel-load-languages
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
              (cl-loop for (latex-name . char) in tex--prettify-symbols-alist
                       for name = (substring latex-name 1)
                       unless (or (not (numberp char)) (gethash name entities))
                       collect (list name
                                     (concat "\\" name)
                                     t
                                     (concat "&#" (number-to-string char) ";")
                                     name
                                     name
                                     (char-to-string char)))))))
  ;; Various preferences
  (setq org-mouse-1-follows-link nil)

  (setq org-log-done t
        org-use-speed-commands t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t

        ;; special ctrl-a/e behaviour
        org-special-ctrl-a/e t
        org-special-ctrl-k t

        org-catch-invisible-edits 'smart
        ;; Number of empty lines needed to keep an empty line between
        ;; collapsed trees.
        org-cycle-separator-lines 2 ;; default = 2
        org-agenda-start-on-weekday nil
        org-agenda-inhibit-startup t       ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-agenda-span 14
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-export-will-product-buffer-when-displayed t
        org-export-with-sub-superscripts t
        org-tags-column -65
        org-ellipsis "  "
        org-hide-emphasis-markers nil
        org-hide-leading-stars nil
        org-hide-block-startup nil
        org-startup-indented nil
        org-adapt-indentation nil
        org-startup-folded 'content
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        org-format-latex-options
        (plist-put org-format-latex-options :scale 1.6)
        org-highlight-latex-and-related '(latex)
        org-src-fontify-natively t
        org-preview-latex-default-process 'imagemagick)

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
           "
* TODO %^{Task}
  Captured: %t
  %?")
          ("i" "Idea" entry
           (file+headline ,(expand-var! "org/*note*") "Idea")
           "
* %^{Idea}
  Captured: %U
  %?")
          ("l" "Link" entry
           (file+headline ,(expand-var! "org/*note*") "Link")
           "
* %^{Link}
  Captured: %U
  %?")
          ("L" "Long term tasks" checkitem
           (file+function
            ,(expand-var! "org/*task*")
            (lambda ()
              (goto-char (point-min))
              (unless (re-search-forward "^\\* Long-term Task")
                (error "Can not find heading `Long-term Task'"))
              (unwind-protect
                  (progn
                    (org-narrow-to-subtree)
                    (let* ((headings (cdr (counsel-outline-candidates)))
                           (result (assoc
                                    (completing-read
                                     "Select subsection: "
                                     headings
                                     nil
                                     :require-match
                                     nil
                                     'org-capture-long-term-task-history)
                                    headings)))
                      (goto-char (cdr result))
                      (end-of-line 1)))
                (widen))))
           "   - [ ] %^{Task} %?")))

  (define-key! :map org-mode-map
    ("C-c t" :map org-table-extra-map)
    ("C-c [" . org-reftex-citation)
    ("C-c {" . org-reftex-citation)
    ("C-c C-x [" . org-agenda-file-to-front)
    ("C-c v" . org/open-pdf)
    ("M-," . org-mark-ring-goto)
    ("M-." . org-mark-ring-push)
    ("M-n" . org/next-item)
    ("M-p" . org/previous-item)
    ("C-c l" . org-store-link)
    ("C-M-i" . completion-at-point)
    ("C-c n" . org-next-block)
    ("C-c p" . org-previous-block)
    ([f5] . org/open-pdf)
    ([C-f9] . org/create-latex-fragemnt-cache)
    ([f9] . org/publish-current-file)
    ([f10] . org-publish)
    ("C-c C-t" . org-todo)))

(with-eval-after-load 'ox-html
  ;; (advice-add 'org-html-do-format-code
  ;;             :around #'org*around-html-do-format-code)

  (setq org-html-head (eval-when-compile
                        (concat "<style type=\"text/css\">\n/*<![CDATA[*/\n"
                                (read-file-content! (expand-etc! "org-manual.css"))
                                "\n/*]]>*/\n</style>")))
  (setq org-html-mathjax-template (eval-when-compile
                                    (read-file-content!
                                     (expand-etc! "org-mathjax-template"))))

  (setcdr (assoc 'scale org-html-mathjax-options) '("90"))
  (setcdr (assoc 'align org-html-mathjax-options) '("center")))

(with-eval-after-load 'ox-latex
  (ignore-errors
    (require 'ox-bibtex))

  (advice-add 'org-publish-file :around #'without-user-record!!)
  (advice-add 'org-latex-compile :after #'org*after-latex-compile)
  (advice-add 'org-latex-link :around #'org*around-latex-link)
  (advice-add 'org-latex--inline-image :around #'org*around-latex-inline-image)

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
    (add-to-list 'org-latex-classes
                 `("cn-article"
                   ,(concat "\\documentclass[11pt,a4paper]{article}\n"
                            common)
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 `("cn-book"
                   ,(concat "\\documentclass[11pt,openany]{book}\n"
                            common)
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))))

(with-eval-after-load 'ox-beamer
  (add-to-list 'org-latex-classes
               `("cn-beamer"
                 ,(s-replace "[ORG-TEMPLATE-DIR]"
                             org-templates-directory
                             (read-file-content!
                              (expand-file-name "beamer" org-templates-directory)))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(define-key!
  ("C-c '" . poporg-dwim)
  ([C-f12] . org-capture))

(provide 'init-org)
