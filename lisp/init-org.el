(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#484848" :background "#020202")))
  "Face used for line delimiting the begin of a src block"
    :group 'org-block)

(defface org-block-background
  '((t (:background "#080808")))
  "Face for the background of a src block"
    :group 'org-block)

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#484848" :background "#020202")))
  "Face used for line delimiting the end of a src block "
  :group 'org-block)

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
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
    (let ((run-spellcheck ad-return-value))
      (if ad-return-value
          (cond
           ((org-mode-is-code-snippet)
            (setq run-spellcheck nil))
           ((org-mode-current-line-is-property)
            (setq run-spellcheck nil))))
      (setq ad-return-value run-spellcheck)))
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
        ;; org-startup-indented t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        )

  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
  (setq org-imenu-depth 9)

  (setq org-src-fontify-natively t)

  (defadvice org-publish (around org-publish-advice activate)
    "Stop running major-mode hook when org-publish"
    (let ((old load-user-customized-major-mode-hook))
      (setq load-user-customized-major-mode-hook nil)
      ad-do-it
      (setq load-user-customized-major-mode-hook old)))

  (defun org-mode-hook-setup ()
    (setq evil-auto-indent nil)
    ;; org-mode's own flyspell will be loaded
    (enable-flyspell-mode-conditionally)

    (flyspell-mode -1)

    (org-bullets-mode 1)

    ;; don't spell check double words
    (setq flyspell-check-doublon nil)

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

(with-eval-after-load 'ox-latex

  (setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                "xelatex -shell-escape -interaction nonstopmode %f"))

  ;;使用minted
  (setq org-latex-listings 'minted)
  (setq org-latex-default-class "cn-article")
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{minted}
\\usepackage{mdframed}
\\usepackage{amsmath}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=green,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont{Ubuntu Mono} %英文缺省
\\setsansfont{Ubuntu Mono} %英文无衬线
\\setmonofont{Ubuntu Mono}% 英文打字机
\\setCJKmainfont{STKaiti} %中文缺省
\\setCJKsansfont{Noto San S Chinese} %中文无衬线
\\setCJKmonofont{STSong}% 中文打字机
\\hypersetup{unicode=true}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\surroundwithmdframed{minted}
\\mdfsetup{
  backgroundcolor=bg
}
\\setminted{
autogobble=true,
breaklines=true,
frame=none,
linenos=true,
tabsize=4
}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(provide 'init-org)
