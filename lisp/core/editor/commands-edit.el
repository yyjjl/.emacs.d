;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;;;###autoload
(defun ymacs-editor/occur-dwim ()
  (interactive)
  (let* ((candidate
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (when-let (sym (thing-at-point 'symbol))
              (concat "\\_<" (regexp-quote sym) "\\_>"))))
         (regexp-history
          (if candidate
              (cons candidate regexp-history)
            regexp-history)))
    (call-interactively 'occur)))

;;;###autoload
(defun ymacs-editor/font-faces-at-point ()
  "Get the font face under cursor."
  (interactive)
  (let* ((pos (point))
         (text (buffer-substring pos (1+ pos)))
         (faces (-uniq (-flatten (list (get-char-property pos 'face)
                                       (get-char-property 0 'face text))))))
    (message "%s" faces)))

;;;###autoload
(defun ymacs-editor/find-file-externally (-files)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (list (buffer-file-name)))))
  (when (or (<= (length -files) 5)
            (y-or-n-p "Open more than 5 files? "))
    (dolist (file -files)
      (counsel-find-file-extern file))))

;;;###autoload
(defun ymacs-editor/narrow-or-widen-dwim (&optional -arg)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument -ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode ymacs-editor-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if -arg (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if -arg #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

;;;###autoload
(defun ymacs-editor/forward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun -n))

;;;###autoload
(defun ymacs-editor/backward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun (- -n)))

(autoload #'python-nav-forward-statement "python")
(autoload #'c-end-of-statement "cc-mode")

;;;###autoload
(defun ymacs-editor/forward-sentence-or-sexp (&optional -n)
  (interactive "^p")
  (cond
   ((and (= (abs -n) 1)
         (char-before)
         (char-after)
         (or (and (> -n 0) (eq (char-syntax (char-after)) ?\())
             (and (< -n 0)
                  (or (eq (char-syntax (char-before)) ?\))
                      (when (eq (char-syntax (char-after)) ?\))
                        (forward-char 1)
                        t)))))
    (forward-sexp -n))

   ((derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (c-end-of-statement -n nil t))

   ((derived-mode-p 'python-mode)
    (python-nav-forward-statement -n))

   ((derived-mode-p 'prog-mode 'org-mode 'latex-mode)
    (condition-case nil
        (forward-sexp -n)
      (scan-error
       (forward-char -n))))
   (t (forward-sentence -n))))

;;;###autoload
(defun ymacs-editor/backward-sentence-or-sexp (&optional -n)
  (interactive "^p")
  (ymacs-editor/forward-sentence-or-sexp (- -n)))

;;;###autoload
(defun ymacs-editor/smart-move-begining-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

;;;###autoload
(defun ymacs-editor/comment-dwim ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun ymacs-editor/iedit-mode (-arg)
  (interactive "P")
  (if (bound-and-true-p iedit-rectangle-mode)
      (iedit-rectangle-mode -1)
    (if (and (equal -arg '(4))
             (bound-and-true-p lsp-mode))
        (lsp-iedit-highlights)
      (call-interactively #'iedit-mode))))

;;;###autoload
(defun ymacs-editor/format-paragraph (&optional -whole-buffer)
  (interactive "P")

  (if (use-region-p)
      (call-interactively #'indent-region)
    (save-mark-and-excursion
      (if -whole-buffer
          (indent-region (point-min) (point-max))
        (mark-paragraph)
        (call-interactively #'indent-region)))))
