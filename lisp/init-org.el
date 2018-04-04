(require-packages!
 ;; (org :compile (org ob ox-html org-table))
 (org-plus-contrib :compile (org ob ox-html org-table))
 ;; IPython notebook feature in `org-mode'
 ob-ipython
 ;; Export colorful src block in `org-mode'
 htmlize
 poporg
 company-auctex)



(defmacro org/by-backend (&rest body)
  `(case (or (and (boundp 'org-export-current-backend)
                  org-export-current-backend)
             'babel)
     ,@body))

(defun org/split-src-block (&optional $below)
  "Split the current src block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
         (language (org-element-property :language el))
         (parameters (org-element-property :parameters el)))
    (beginning-of-line)
    (insert (format "#+end_src\n#+begin_src %s %s\n" language parameters))
    (beginning-of-line)
    (when (not $below)
      (org-babel-previous-src-block))))

(defvar org--ipython-parent-buffer nil)
(defvar org--ipython-src-block nil)
(defvar-local org--ipython-error-line 0)

(defun org%ipython-trace-move ($n)
  (let ((search-func (if (> $n 0)
                         #'re-search-forward
                       (setq $n (- 0 $n))
                       #'re-search-backward))
        found)
    (while (and (> $n 0)
                (setq found
                      (apply search-func '("^-+> \\([0-9]+\\)" nil t))))
      (setq $n (1- $n)))
    found))

(defun org/ipython-trace-prev (&optional $n)
  (interactive "P")
  (unless (org%ipython-trace-move (or (and $n (- 0 $n)) -1))
    (message "No previous frame")))

(defun org/ipython-trace-next (&optional $n)
  (interactive "P")
  (unless (org%ipython-trace-move (or $n 1))
    (message "No next frame")))

(defun org/ipython-jump ($lineno &optional $do-jump)
  (interactive (list (or org--ipython-error-line 0) t))
  (if (and (buffer-live-p org--ipython-parent-buffer)
           org--ipython-src-block)
      (let ((p (org-babel-where-is-src-block-head org--ipython-src-block))
            (window (if $do-jump
                        (progn (pop-to-buffer org--ipython-parent-buffer)
                               (selected-window))
                      (display-buffer org--ipython-parent-buffer))))
        (with-selected-window window
          (goto-char p)
          (forward-line $lineno)
          (recenter)
          (point)))
    (message "Parent buffer killed or Can not find src block !!!")))

(defun org/ipython-trace-bury-buffer ()
  (interactive)
  (org/ipython-jump org--ipython-error-line)
  (call-interactively 'quit-window))

(defun org*ipython-before-execute (&rest $args)
  (setq org--ipython-parent-buffer (current-buffer))
  (setq org--ipython-src-block (org-element-context)))

(advice-add 'org-babel-execute:ipython
            :before #'org*ipython-before-execute)

(defun org*ipython-trace-setup ($fn &rest $args)
  (with-current-buffer (apply $fn $args)
    (use-local-map (copy-keymap special-mode-map))
    (define-key! :map (current-local-map)
      ("q" . org/ipython-trace-bury-buffer)
      ("p" . org/ipython-trace-prev)
      ("n" . org/ipython-trace-next)
      ("j" . org/ipython-jump))

    (goto-char (point-min))
    (if (re-search-forward "-+> \\([0-9]+\\)" nil t)
        (setq org--ipython-error-line (string-to-number (match-string 1)))
      (goto-char (point-min))
      (when (re-search-forward "SyntaxError:" nil t)
        (goto-char (point-min))
        ;; Get the line number
        (when (re-search-forward "File.*, line \\([0-9]+\\)" nil t)
          (goto-char (match-end 0))
          (setq org--ipython-error-line (string-to-number (match-string 1))))))
    (goto-char (point-min))
    (setq header-line-format
          (format "Error at line: %d, press `j' to jump to location"
                  org--ipython-error-line))))

(advice-add 'ob-ipython--create-traceback-buffer
            :around #'org*ipython-trace-setup)

(with-eval-after-load 'ob
  (define-key! :map org-babel-map
    ("/" . org/split-src-block)))

(with-eval-after-load 'poporg
  (add-to-list 'shackle-rules
               '(poporg-mode :size 0.5 :select t)))

(with-eval-after-load 'ob-ipython
  ;; Don't prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)

  (define-hook! org|babel-after-execute
    ((org-babel-after-execute-hook :append))
    (dolist (buf (buffer-list))
      (when (and (string-prefix-p " *http " (buffer-name buf))
                 (let ((proc (get-buffer-process buf)))
                   (not (and proc (process-live-p proc)))))
        (kill-buffer buf)))
    ;; Display/update images in the buffer after I evaluate
    (org-display-inline-images t)))

(defun org/open-pdf (&optional $arg)
  (interactive "P")
  (let* ((-fn (buffer-file-name))
         (fn (and -fn
                  (concat (file-name-sans-extension -fn)
                          ".pdf"))))
    (if (and fn (not $arg) (file-exists-p fn))
        (find-file fn)
      (counsel-find-file (file-name-base -fn)))))

;; Do not load extra modules
(setq org-modules '(org-info))
;; Do not load extra backends
(setq org-export-backends '(html latex beamer))
(setq org-mouse-1-follows-link nil)
(defvar org-table-extra-map
  (define-key! :map (make-sparse-keymap)
    ("t" . orgtbl-insert-radio-table)
    ("c" . org-table-create)
    ("I" . org-table-import)
    ("e" . org-table-export)
    ("d" . org-table-delete-column)
    ("i" . org-table-insert-column)
    ("r" . org-table-show-reference)))
(with-eval-after-load 'org
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
         prepend t)))
   'append)

  (setq org-structure-template-alist
        (--map (list (car it) (downcase (cadr it)))
               org-structure-template-alist))
  (setq org-entities-user
        '(("rangle" "\\rangle" t "&rangle;" "rangle" "rangle" "❭")
          ("langle" "\\langle" t "&langle;" "langle" "langle" "❬")
          ("doteq" "\\doteq" t "&doteq;" "doteq" "doteq" "≐")
          ("lessdot" "\\lessdot" t "&lessdot;" "lessdot" "lessdot" "⋖")
          ("gtrdot" "\\gtrdot" t "&gtrdot;" "gtrdot" "gtrdot" "⋗")))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (python . t)
                                 (emacs-lisp . t)
                                 (dot . t)
                                 (js . t)
                                 (perl . t)
                                 (latex . t)
                                 (haskell . t)))
  ;; Various preferences
  (setq org-log-done t
        org-use-speed-commands t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
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
        org-export-kill-product-buffer-when-displayed t
        org-export-with-sub-superscripts t
        org-tags-column -65
        org-ellipsis "  "
        org-hide-emphasis-markers nil
        org-hide-leading-stars t
        org-hide-block-startup t
        ;; org-startup-indented t
        org-startup-folded 'content
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        org-format-latex-options
        (plist-put org-format-latex-options :scale 1.6)
        org-highlight-latex-and-related '(latex)
        org-src-fontify-natively t
        org-preview-latex-default-process 'imagemagick)

  (add-to-list 'org-babel-tangle-lang-exts '("ipython" . "py"))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

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
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)"
                          "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
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

  (unless (featurep 'company-auctex)
    (require 'company-auctex))
  (defun company-org-symbols ($command &optional $arg &rest $ignored)
    "Complete math symbol in LaTeX fragments, better than
`pcomplete'"
    (interactive (list 'interactive))
    (cl-case $command
      (interactive (company-begin-backend 'company-org-symbols))
      (prefix (ignore-errors (and (org-inside-LaTeX-fragment-p)
                                  (company-grab-word))))
      (candidates (company-auctex-symbol-candidates $arg))
      (annotation (company-auctex-symbol-annotation $arg))))

  (define-hook! org|setup (org-mode-hook)
    (auto-fill-mode -1)

    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions
                 '(lambda () (ignore-errors pcomplete-completions-at-point)))
    (add-to-list 'company-backends 'company-org-symbols))

  (define-hook! org|src-setup (org-src-mode-hook)
    (when (eq major-mode 'python-mode)
      ;; company-ob-ipython is very slow
      (setq-local company-idle-delay nil)
      (add-to-list 'company-backends #'company-ob-ipython)
      (local-set-key (kbd "C-c C-.") #'ob-ipython-inspect))
    (flycheck-mode -1))

  (defun org/next-item (&optional $n)
    (interactive "p")
    (let ((cmd (if (> $n 0) #'org-next-item #'org-previous-item))
          (col (current-column))
          (n (abs $n)))
      (condition-case err
          (while (>= (decf n) 0)
            (funcall cmd))
        (error (message "%s" err)))
      (move-to-column col)))

  (defun org/previous-item (&optional $n)
    (interactive "p")
    (org/next-item (- $n)))

  (defun org*fill-paragraph-hack (&rest _)
    (when (fboundp #'extra/insert-space-around-chinese)
      (let ((element (org-element-at-point)))
        (extra/insert-space-around-chinese
         (min (point-max)
              (org-element-property :contents-end element))
         (max (point-min)
              (org-element-property :contents-begin element))))))
  (advice-add 'org-fill-paragraph :after #'org*fill-paragraph-hack)

  (define-key org-mode-map (kbd "C-c t") org-table-extra-map)
  (define-key! :map org-mode-map
    ("C-c C-." . ob-ipython-inspect)
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
    ([f5] . org/open-pdf)
    ([f9] . (lambda () (interactive)
              (save-excursion
                (let ((core--buffer-useful nil))
                  (org-publish-current-file)))))
    ([f10] . org-publish)
    ("C-c C-t" . org-todo)))

(with-eval-after-load 'ox-html
  (defun org*html-export-with-line-number ($fn &rest $rest)
    (when (= (length $rest) 5)
      (let ((num-start (nth 4 $rest)))
        (unless num-start
          (setq num-start 0))
        (setcar (nthcdr 4 $rest) num-start)))
    (apply $fn $rest))

  ;; (advice-add 'org-html-do-format-code
  ;;             :around #'org*html-export-with-line-number)
  (setq org-html-mathjax-template (eval-when-compile
                                    (read-file-content!
                                     (expand-etc! "org-mathjax-template"))))
  (setcdr (assoc 'scale org-html-mathjax-options) '("90"))
  (setcdr (assoc 'align org-html-mathjax-options) '("center")))

(with-eval-after-load 'ox-latex
  (ignore-errors
    (require 'ox-bibtex))

  (defun org*latex-link-hack ($fn $link $desc $info)
    (let* ((type (org-element-property :type $link))
           (raw-path (org-element-property :path $link))
           (search-option (org-element-property :search-option $link))
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
                  (or $desc ""))
        (funcall $fn $link $desc $info))))
  (advice-add 'org-latex-link :around #'org*latex-link-hack)

  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("latexmk -g -pdf -dvi- -ps- -pdflatex=\"xelatex -shell-escape %%O %%S\" -outdir=%o %f"))

  ;; Use minted
  (setq org-latex-listings 'minted)
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-default-class "cn-article")
  (setq org-latex-packages-alist
        '(("" "ctex" nil)
          ("" "setspace,dcolumn" t)
          ("" "subfig" nil)
          ("" "hyperref" t)
          ("" "graphicx,psfrag,epsfig" t)
          ("" "minted" nil)
          ("" "mdframed" nil)
          ("" "amsmath, amsfonts, amssymb, amsthm, bm, upgreek" t)
          ("mathscr" "eucal" t)
          ("" "geometry" t)
          ("" "tcolorbox" t)))
  (let ((common (read-file-content!
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
                             (read-file-content!
                              (expand-file-name "beamer" org-template-directory)))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(define-key!
  ("C-c '" . poporg-dwim)
  ([C-f12] . org-capture))

(provide 'init-org)
