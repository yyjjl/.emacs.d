(with-eval-after-load 'ob-ipython
  ;; Don't prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)

  (defhook org|babel-after-execute ((org-babel-after-execute-hook :append))
    (dolist (buf (buffer-list))
      (when (and (string-prefix-p " *http " (buffer-name buf))
                 (let ((proc (get-buffer-process buf)))
                   (not (and proc (process-live-p proc)))))
        (kill-buffer buf)))
    ;; Display/update images in the buffer after I evaluate
    (org-display-inline-images t))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t))))

;; Do not load extra modules
(setq org-modules '(org-info))
;; Do not load extra backends
(setq org-export-backends '(ascii html latex beamer))
(setq org-mouse-1-follows-link nil)
(with-eval-after-load 'org
  ;; No spell check for embedded snippets
  ;; No spell check for property
  ;; Please note flyspell only use ispell-word
  (defun org|flyspell-verify (fn &rest args)
    (and (apply fn args)
         (not (org-in-src-block-p))
         (not (org-at-property-p))))
  (advice-add 'org-mode-flyspell-verify :around #'org|flyspell-verify)

  ;; Various preferences
  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-catch-invisible-edits 'smart
        ;; Number of empty lines needed to keep an empty line between
        ;; collapsed trees.
        org-cycle-separator-lines 2     ; default = 2

        org-agenda-start-on-weekday nil
        org-agenda-inhibit-startup t       ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-agenda-span 14
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window

        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        org-export-with-sub-superscripts t
        org-tags-column 80
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        ;; org-startup-indented t
        ;; org-pretty-entities t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-highlight-latex-and-related '(latex)
        org-src-fontify-natively t)

  ;; Refile targets include this file and any file contributing to the
  ;; agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows
  ;; the next level of targets etc
  (setq org-outline-path-complete-in-steps t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)"
                          "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
  (setq org-imenu-depth 9)

  (unless (featurep 'company-auctex)
    (require 'company-auctex))
  (defun company-org-symbols (command &optional arg &rest ignored)
    "Complete math symbol in LaTeX fragments, better than
`pcomplete'"
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-org-symbols))
      (prefix  (and (org-inside-LaTeX-fragment-p)
                    (company-grab-word)))
      (candidates (company-auctex-symbol-candidates arg))
      (annotation (company-auctex-symbol-annotation arg))))

  (defhook org|setup (org-mode-hook)
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions
                 'pcomplete-completions-at-point)
    (add-to-list 'company-backends 'company-org-symbols)
    (setq-local company-backends (remove 'company-dabbrev company-backends))
    ;; Display wrapped lines instead of truncated lines
    (setq truncate-lines nil)
    (setq word-wrap t))

  (add-hook 'org-src-mode-hook #'(lambda () (flycheck-mode -1)))

  (define-keys :map org-mode-map
    ("C-c I" . ob-ipython-inspect)
    ("C-c c i" . org-clock-in)
    ("C-c c o" . org-clock-out)
    ("C-c c c" . org-clock-in-last)
    ("C-c c e" . org-clock-modify-effort-estimate)
    ("C-c c q" . org-clock-cancel)
    ("C-c c g" . org-clock-goto)
    ("C-c c d" . org-clock-display)
    ("C-c c r" . org-clock-report)
    ("M-," . org-mark-ring-goto)
    ("M-." . org-mark-ring-push)
    ("C-c l" . org-store-link)
    ([f9] . org-publish-current-file)
    ([f10] . org-publish)
    ([f5] . org-present)
    ("C-c t" . org-todo)
    ("C-c C-t" . nil)))

(with-eval-after-load 'org-clock
  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))


(with-eval-after-load 'ox-html
  (defun org|html-export-with-line-number (fn &rest rest)
    (when (= (length rest) 5)
      (let ((num-start (nth 4 rest)))
        (unless num-start
          (setq num-start 0))
        (setcar (nthcdr 4 rest) num-start)))
    (apply fn rest))

  ;; (advice-add 'org-html-do-format-code
  ;;             :around #'org|html-export-with-line-number)
  
  (setcdr (assoc 'scale org-html-mathjax-options) '("90"))
  (setcdr (assoc 'align org-html-mathjax-options) '("left"))
  (setcdr (assoc 'path org-html-mathjax-options)
          '("../../node_modules/mathjax/MathJax.js")))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "xelatex -shell-escape -interaction nonstopmode %f"))

  ;; Use minted
  (setq org-latex-listings 'minted)
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
  (let ((common (read-file-as-string
                 (expand-file-name "common" org-template-directory))))
    (add-to-list 'org-latex-classes
                 `("cn-article"
                   ,(concat "\\documentclass[11pt,a4paper]{article}\n"
                            common)
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list  'org-latex-classes
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
                             org-template-directory
                             (read-file-as-string
                              (expand-file-name "beamer" org-template-directory)))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(with-eval-after-load 'org-present
  (define-keys :map org-present-mode-keymap
    ("<down>" . scroll-down-line)
    ("<up>" . scroll-up-line)
    ("q" . org-present-quit))
  (setq org-present-text-scale 2)

  (defhook org|present-setup (org-present-mode-hook)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
    (setq mode-line-format nil)
    (org-present-big)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only))
  (defhook org|present-exit (org-present-mode-quit-hook)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 1.5))
    (setq mode-line-format mode-line|default-format)
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)))

(provide 'init-org)
