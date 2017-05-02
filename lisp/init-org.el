(with-eval-after-load 'org
  ;; {{ NO spell check for embedded snippets
  (defun org-mode-is-code-snippet ()
    (let (rlt
          (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
          (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
          (old-flag case-fold-search)
          b e)
      (save-excursion
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt t))
      rlt))

  ;; no spell check for property
  (defun org-mode-current-line-is-property ()
    (let (cur-line)
      (setq cur-line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
      ;; (message "cur-line=%s" cur-line)
      (string-match "^[ \t]+:[A-Z]+:[ \t]+" cur-line)))

  ;; Please note flyspell only use ispell-word
  (defun org-mode-flyspell-verify-hack ()
    (let ((run-spellcheck ad-return-value))
      (if ad-return-value
          (cond
           ((org-mode-is-code-snippet)
            (setq run-spellcheck nil))
           ((org-mode-current-line-is-property)
            (setq run-spellcheck nil))))
      (setq ad-return-value run-spellcheck)))
  (advice-add 'org-mode-flyspell-verify :after
              #'org-mode-flyspell-verify-hack)
  ;; }}

  ;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
  (defun narrow-or-widen-dwim ()
    "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
    (interactive)
    (cond ((buffer-narrowed-p) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((equal major-mode 'org-mode) (org-narrow-to-subtree))
          (t (error "Please select a region to narrow to"))))

  ;; Various preferences
  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-agenda-start-on-weekday nil
        org-agenda-span 14
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        ;; org v7
        org-export-odt-preferred-output-format "doc"
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80
        org-latex-preview-ltxpng-directory "~/.emacs.d/data/ltxpng/"
        ;; org-startup-indented t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t       ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        ;; org-pretty-entities t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 5)
                                   (org-agenda-files :maxlevel . 5))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)"
                          "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
  (setq org-imenu-depth 9)

  (setq org-src-fontify-natively t)

  (defun org-mode-hook-setup ()
    ;; org-mode's own flyspell will be loaded
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions
                 'pcomplete-completions-at-point)
    (flyspell-mode -1)

    ;; display wrapped lines instead of truncated lines
    (setq truncate-lines nil)
    (setq word-wrap t)
    (turn-on-stripe-table-mode))

  (add-hook 'org-mode-hook 'org-mode-hook-setup)

  (bind-keys :map org-mode-map
             ("C-c c i" . org-clock-in)
             ("C-c c o" . org-clock-out)
             ("C-c c c" . org-clock-in-last)
             ("C-c c e" . org-clock-modify-effort-estimate)
             ("C-c c q" . org-clock-cancel)
             ("C-c c g" . org-clock-goto)
             ("C-c c d" . org-clock-display)
             ("C-c c r" . org-clock-report)
             ([f9] . org-project-publish-this-file-to-html)
             ([f10] . org-publish)
             ("C-c t" . org-todo)
             ("C-c C-t" . nil)))


(with-eval-after-load 'org-clock
  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line))


(setq outline-minor-mode-prefix "\C-co")
(with-eval-after-load 'outline
  (let ((map (make-sparse-keymap)))
    (bind-keys :map map
               ("a" . show-all)
               ("b" . outline-backward-same-level)
               ("f" . outline-forward-same-level)
               ("h" . hide-entry)
               ("d" . hide-subtree)
               ("s" . show-entry)
               ("n" . outline-next-visible-heading)
               ("p" . outline-previous-visible-heading)
               ("k" . show-branches)
               ("l" . hide-leaves)
               ("o" . hide-other)
               ("q" . hide-sublevels)
               ("x" . show-subtree)
               ("t" . hide-body)
               ("u" . outline-up-heading)
               ("v" . outline-move-subtree-down)
               ("6" . outline-move-subtree-up)
               ("<" . outline-promote)
               (">" . outline-demote)
               ("m" . outline-mark-subtree))
    (define-key outline-minor-mode-map outline-minor-mode-prefix
      map)))

(defvar org-latex-extra-options
  (string-join
   '("[NO-DEFAULT-PACKAGES]\n[PACKAGES]"
     ;;         "\\setmonofont{Monaco}"
     ;;         "\\setmainfont{Ubuntu}"
     "%\\setCJKmainfont{KaiTi}"
     "\\definecolor{codeblockbg}{rgb}{0.95,0.95,0.95}"
     "\\definecolor{linkcolor}{rgb}{0.1,0.3,0.5}"
     "\\hypersetup{colorlinks=true,linkcolor=linkcolor}"
     "\\geometry{top=2.54cm,bottom=2.54cm,left=2.54cm,right=2.54cm}"
     "\\renewcommand{\\theFancyVerbLine}{"
     "  \\sffamily \\textcolor[rgb]{1.0,0.2,1.0}{"
     "   \\scriptsize \\oldstylenums{\\arabic{FancyVerbLine}}}}"
     "\\renewcommand\\listoflistingscaption{Program Code List}"
     "\\renewcommand\\listingscaption{Program Code}"
     "\\surroundwithmdframed{minted}"
     "\\surroundwithmdframed{quote}"
     "\\mdfsetup{"
     "topline=false,bottomline=false,rightline=false,"
     "linewidth=1pt,linecolor=black,"
     "backgroundcolor=codeblockbg"
     "}"
     "\\setminted{"
     ;;         "fontsize=\\scriptsize,"
     "fontsize=\\small,"
     "autogobble=true,breaklines=true,frame=none,linenos=true,tabsize=4,"
     "breakautoindent=false,mathescape=true,escapeinside=\\#\\%,"
     "breaksymbolleft=\\raisebox{0.8ex}{"
     "\\rotatebox{90}{\\small\\ensuremath{\\curvearrowleft}}},"
     "breaksymbolindentleft=0pt,"
     "breaksymbolsepleft=0pt,"
     "breaksymbolright=\\rotatebox{270}{\\small\\ensuremath{\\curvearrowright}},"
     "breaksymbolindentright=0pt,"
     "breaksymbolsepright=0pt"
     "}"
     "\\definecolor{codeblockbg}{rgb}{0.95,0.95,0.95}"
     "\\definecolor{linkcolor}{rgb}{0.1,0.3,0.5}")
   "\n"))

(defvar org-latex-section-options
  (string-join '("\\definecolor{sectioncolor}{rgb}{0.5,0.5,0.25}"
                 "\\definecolor{subsubsectioncolor}{rgb}{0.7,0.3,0.2}"
                 "\\sectionfont{\\LARGE\\color{sectioncolor}}"
                 "\\subsectionfont{\\Large\\color{cyan}}"
                 "\\subsubsectionfont{\\large\\color{subsubsectioncolor}}"
                 "\\paragraphfont{\\large}")
               "\n"))

(with-eval-after-load 'ox-latex
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "xelatex -shell-escape -interaction nonstopmode %f"))

  ;;使用minted
  (when (executable-find "pygmentize")
    (setq org-latex-listings 'minted))
  (setq org-latex-default-class "cn-article")
  (setq org-latex-packages-alist
        '(("" "ctex" nil)
          ("" "setspace,dcolumn" t)
          ("" "subfig" nil)
          ("" "hyperref" t)
          ("" "graphicx,psfrag,epsfig" t)
          ("" "minted" nil)
          ("" "mdframed" nil)
          ("" "amsmath,amsfonts,amssymb,amsthm,bm,upgreek" t)
          ("mathscr" "eucal" t)
          ("" "geometry" t)))
  (add-to-list  'org-latex-classes
                (list "cn-article"
                      (concat "\\documentclass[11pt,a4paper]{article}\n"
                              "\\usepackage{sectsty}\n"
                              org-latex-extra-options "\n"
                              org-latex-section-options)
                      '("\\section{%s}" . "\\section*{%s}")
                      '("\\subsection{%s}" . "\\subsection*{%s}")
                      '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      '("\\paragraph{%s}" . "\\paragraph*{%s}")
                      '("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list  'org-latex-classes
                (list "cn-book"
                      (concat "\\documentclass[11pt,openany]{book}\n"
                              "\\usepackage{sectsty}\n"
                              org-latex-extra-options "\n"
                              org-latex-section-options)
                      '("\\chapter{%s}" . "\\chapter*{%s}")
                      '("\\section{%s}" . "\\section*{%s}")
                      '("\\subsection{%s}" . "\\subsection*{%s}")
                      '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      '("\\paragraph{%s}" . "\\paragraph*{%s}"))))

(with-eval-after-load 'ox-beamer
  (add-to-list  'org-latex-classes
                (list "cn-beamer"
                      (concat "\\documentclass[bigger]{beamer}\n"
                              org-latex-extra-options)
                      '("\\section{%s}" . "\\section*{%s}")
                      '("\\subsection{%s}" . "\\subsection*{%s}")
                      '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      '("\\paragraph{%s}" . "\\paragraph*{%s}")
                      '("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'init-org)
