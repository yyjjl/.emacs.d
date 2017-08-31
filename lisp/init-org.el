;; IPython notebook feature in `org-mode'
(require! 'ob-ipython)
(require! 'ox-pandoc)
(require! 'org-bullets)
;; Export colorful src block in `org-mode'
(require! 'htmlize)
(require! 'company-auctex)



(defun org/split-src-block (&optional below)
  "Split the current src block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
         (language (org-element-property :language el))
         (parameters (org-element-property :parameters el)))
    (beginning-of-line)
    (insert (format "#+END_SRC\n#+BEGIN_SRC %s %s\n" language parameters))
    (beginning-of-line)
    (when (not below)
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
      (let ((p (org-element-property :begin org--ipython-src-block))
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

(with-eval-after-load 'ob-ipython
  ;; Fix encoding error
  (defun ob-ipython--inspect-request (code &optional pos detail)
    (let ((url-request-data (encode-coding-string
                             (json-encode `((code . ,code)
                                            (pos . ,(or pos (length code)))
                                            (detail . ,(or detail 0))))
                             'utf-8))
          (url-request-method "POST"))
      (with-current-buffer (url-retrieve-synchronously
                            ;; TODO: hardcoded the default session here
                            (format "http://%s:%d/inspect/default"
                                    ob-ipython-driver-hostname
                                    ob-ipython-driver-port))
        (if (>= (url-http-parse-response) 400)
            (ob-ipython--dump-error (buffer-string))
          (goto-char url-http-end-of-headers)
          (let ((json-array-type 'list))
            (json-read))))))

  ;; Don't prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)

  (define-hook! org|babel-after-execute ((org-babel-after-execute-hook :append))
    (dolist (buf (buffer-list))
      (when (and (string-prefix-p " *http " (buffer-name buf))
                 (let ((proc (get-buffer-process buf)))
                   (not (and proc (process-live-p proc)))))
        (kill-buffer buf)))
    ;; Display/update images in the buffer after I evaluate
    (org-display-inline-images t)))

(setq org-bullets-bullet-list '("#"))
;; Do not load extra modules
(setq org-modules '(org-info))
;; Do not load extra backends
(setq org-export-backends '(html latex beamer pandoc))
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
      (2 (progn
           (put-text-property (match-beginning 2) (match-end 2)
                              'display '((raise 0.5)))
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

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (python . t)
                                 (emacs-lisp . t)
                                 (dot . t)
                                 (js . t)
                                 (sh . t)
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
        org-agenda-inhibit-startup t ;; ~50x speedup
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
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-highlight-latex-and-related '(latex)
        org-src-fontify-natively t)

  (add-to-list 'org-babel-tangle-lang-exts '("ipython" . "py"))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

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
      (prefix (and (org-inside-LaTeX-fragment-p)
                   (company-grab-word)))
      (candidates (company-auctex-symbol-candidates arg))
      (annotation (company-auctex-symbol-annotation arg))))


  (define-hook! org|setup (org-mode-hook)
    ;; Fix font-lock bug in `org-mode' 8.2.10
    (let ((macros (assoc "{{{.+}}}" org-font-lock-keywords)))
      (when macros
        (setcar macros "{{{.+?}}}")))

    (org-bullets-mode 1)
    (auto-fill-mode -1)

    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions
                 'pcomplete-completions-at-point)
    (add-to-list 'company-backends 'company-org-symbols))

  (define-hook! org|src-setup (org-src-mode-hook)
    (when (eq major-mode 'python-mode)
      (local-set-key (kbd "C-c h") #'ob-ipython-inspect))
    (flycheck-mode -1))

  (define-key org-mode-map (kbd "C-c t") org-table-extra-map)
  (define-key! :map org-mode-map
    ("C-c h" . ob-ipython-inspect)
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
    ([f9] . (lambda () (interactive)
              (save-excursion
                (org-publish-current-file))))
    ([f10] . org-publish)
    ([f5] . org-present)
    ("C-c C-t" . org-todo)))

(with-eval-after-load 'ox-html
  (defun org*html-export-with-line-number (fn &rest rest)
    (when (= (length rest) 5)
      (let ((num-start (nth 4 rest)))
        (unless num-start
          (setq num-start 0))
        (setcar (nthcdr 4 rest) num-start)))
    (apply fn rest))

  ;; (advice-add 'org-html-do-format-code
  ;;             :around #'org*html-export-with-line-number)

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

(provide 'init-org)
