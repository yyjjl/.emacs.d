;;; -*- lexical-binding: t; -*-

;;;###autoload
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

;;;###autoload
(defun org/previous-item (&optional $n)
  (interactive "p")
  (org/next-item (- $n)))

;;;###autoload
(defun company-org-symbols ($command &optional $arg &rest _)
  "Complete math symbol in LaTeX fragments, better than `pcomplete'"
  (interactive (list 'interactive))
  (cl-case $command
    (interactive (company-begin-backend 'company-org-symbols))
    (prefix (ignore-errors (let ((word (company-grab-word)))
                             (and (= ?\\ (char-before (- (point) (length word))))
                                  word))))
    (candidates (company-auctex-symbol-candidates $arg))
    (annotation (company-auctex-symbol-annotation $arg))))

;;;###autoload
(defun org/open-pdf (&optional $arg)
  (interactive "P")
  (let* ((-fn (buffer-file-name))
         (fn (and -fn
                  (concat (file-name-sans-extension -fn)
                          ".pdf"))))
    (if (and fn (not $arg) (file-exists-p fn))
        (find-file fn)
      (counsel-find-file (file-name-base -fn)))))

;;;###autoload
(defun org/latexmk-start-watching ($arg)
  (interactive "P")
  (let* ((file-name (if $arg
                        (read-file-name "LaTeX file to watch"
                                        nil nil :mustmatch)
                      (buffer-file-name)))
         (base-name (file-name-base file-name))
         (out-dir (or (file-name-directory file-name) "./")))
    (let ((proc (get-process "latexmk")))
      (when (process-live-p proc)
        (if (yes-or-no-p "Process latexmk is active, kill it?")
            (progn
              (kill-process proc)
              (sit-for 0.1))
          (error "Process latexmk is still active")))
      (start-process
       "latexmk" (get-buffer-create "LaTeXMK")
       "latexmk" "-quiet" "-pvc" "-g" "-pdf" "-dvi-" "-ps-"
       "-pdflatex=xelatex -shell-escape -interaction=nonstopmode %O %S"
       (concat "-outdir=" out-dir)
       (concat base-name ".tex"))
      (message "latexmk started ..."))))

;;;###autoload
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

;;;###autoload
(defun org/create-latex-fragemnt-cache ($directory)
  (interactive "D")
  (let ((ltximg-dirs (make-hash-table :test 'equal))
        directory-conflict-p
        files)
    (dolist (file (directory-files-recursively $directory "\\.org\\'"))
      (without-user-record!
       (with-current-buffer (find-file-noselect file)
         (let* ((prefix (concat org-preview-latex-image-directory "org-ltximg"))
                (absolute-dir (expand-file-name org-preview-latex-image-directory))
                (image-files (make-hash-table :test 'equal))
                (delete-invalid-p (not (gethash absolute-dir ltximg-dirs))))
           (save-excursion
             (save-restriction
               (widen)
               (if delete-invalid-p
                   (puthash absolute-dir t ltximg-dirs)
                 (setq directory-conflict-p t))
               (org-remove-latex-fragment-image-overlays)
               (org-format-latex prefix
                                 (point-min) (point-max)
                                 default-directory
                                 'overlays
                                 (concat "Create %s fragment for `" file "'")
                                 'forbuffer
                                 org-preview-latex-default-process)
               (dolist (ov (org--list-latex-overlays))
                 (-when-let (image-file
                             (plist-get (cdr-safe (overlay-get ov 'display)) :file))
                   (puthash (expand-file-name image-file
                                              org-preview-latex-image-directory)
                            t
                            image-files)))
               (when (and delete-invalid-p
                          (file-exists-p absolute-dir))
                 (dolist (image-file
                          (directory-files absolute-dir
                                           :full "\\.\\(png\\|jpe?g\\|svg\\)\\'"))
                   (unless (gethash image-file image-files)
                     (push image-file files))))))))))
    (when (and files
               (yes-or-no-p (format "Delete %d invalid cache? " (length files))))
      (dolist (file files)
        (delete-file file))
      (and directory-conflict-p
           (message "Directories for caching have confict !!")))))
