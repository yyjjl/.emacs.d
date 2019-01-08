;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun org/next-item (&optional -n)
  (interactive "p")
  (cond ((org-in-block-p '("src" "quote" "example"))
         (org-next-block (abs -n) (< -n 0)))
        ((org-in-item-p)
         (let ((cmd (if (> -n 0) #'org-next-item #'org-previous-item))
               (col (current-column))
               (n (abs -n)))
           (condition-case err
               (progn
                 (while (>= (decf n) 0)
                   (funcall cmd))
                 (move-to-column col))
             (error (message "%s" err)))))
        (t
         (let ((cmd (if (> -n 0)
                        #'org-next-visible-heading
                      #'org-previous-visible-heading)))
           (funcall cmd (abs -n))))))

;;;###autoload
(defun org/previous-item (&optional -n)
  (interactive "p")
  (org/next-item (- -n)))

;;;###autoload
(defun company-org-symbols (-command &optional -arg &rest _)
  "Complete math symbol in LaTeX fragments, better than `pcomplete'"
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'company-org-symbols))
    (prefix (ignore-errors (let ((word (company-grab-word)))
                             (and (= ?\\ (char-before (- (point) (length word))))
                                  word))))
    (candidates (company-auctex-symbol-candidates -arg))
    (annotation (company-auctex-symbol-annotation -arg))))

;;;###autoload
(defun org/open-pdf (&optional -arg)
  (interactive "P")
  (let* ((-fn (buffer-file-name))
         (fn (and -fn
                  (concat (file-name-sans-extension -fn)
                          ".pdf"))))
    (if (and fn (not -arg) (file-exists-p fn))
        (find-file fn)
      (counsel-find-file (file-name-base -fn)))))

;;;###autoload
(defun org/latexmk-start-watching (-arg)
  (interactive "P")
  (let* ((file-name (if -arg
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
(defun org/split-src-block (&optional -below)
  "Split the current src block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
         (language (org-element-property :language el))
         (parameters (org-element-property :parameters el)))
    (beginning-of-line)
    (insert (format "#+end_src\n#+begin_src %s %s\n" language parameters))
    (beginning-of-line)
    (when (not -below)
      (org-babel-previous-src-block))))

;;;###autoload
(defun org/create-latex-fragemnt-cache (-files -clean-invalid-p)
  (interactive
   (list
    (if (and (not current-prefix-arg)
             (string-suffix-p ".org" (buffer-file-name)))
        ;; create for current file
        (list (buffer-file-name))
      (let ((dir-name
             (if (equal current-prefix-arg '(4))
                 default-directory      ; create for files in current directory
               (read-file-name "File or Directory: "))))
        (unless (file-directory-p dir-name)
          (error "%s is not a directory" dir-name))
        (directory-files-recursively dir-name "\\.org\\'")))
    current-prefix-arg))
  (let ((image-files (make-hash-table :test 'equal))
        (image-dirs (make-hash-table :test 'equal)))
    (dolist (file -files)
      (without-user-record!
       (with-current-buffer (find-file-noselect file)
         (let* ((prefix (concat org-preview-latex-image-directory "org-ltximg"))
                (image-dir (expand-file-name org-preview-latex-image-directory)))
           (puthash image-dir t image-dirs)
           (save-excursion
             (save-restriction
               (widen)
               (org-remove-latex-fragment-image-overlays)
               (org-format-latex prefix
                                 (point-min) (point-max)
                                 default-directory
                                 'overlays
                                 (concat "The %s for `" file "'")
                                 'forbuffer
                                 org-preview-latex-default-process)
               (dolist (ov (org--list-latex-overlays))
                 (-when-let (image-file
                             (plist-get (cdr-safe (overlay-get ov 'display)) :file))
                   (puthash (expand-file-name image-file
                                              org-preview-latex-image-directory)
                            t
                            image-files)))))))))
    (if -clean-invalid-p
        (let ((count 0))
          (cl-loop for image-dir being the hash-keys of image-dirs
                   when (file-exists-p image-dir)
                   do (dolist (image-file
                               (directory-files image-dir
                                                :full "\\.\\(png\\|jpe?g\\|svg\\)\\'"))
                        (unless (gethash image-file image-files)
                          (incf count)
                          (ignore-errors (delete-file image-file)))))
          (message "Deleted %d invalid cache files" count))
      (message "Invalid cache files were not checked"))))
