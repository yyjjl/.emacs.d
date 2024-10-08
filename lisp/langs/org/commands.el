;;; -*- lexical-binding: t; -*-

(require 'org-download)
(require 'transient)

(eval-when-compile
  (require 'org)
  (require 'ox-publish))

(require 'latex)
(defvar org-export-dispatch-last-action)
(defvar org-publish-project-alist)
(defvar org-publish-use-timestamps-flag)

(declare-function org-download-rename-last 'org-download)
(declare-function org-publish-file 'org-publish)
(declare-function org-publish-get-project-from-filename 'org-publish)
(declare-function org-publish-property 'org-publish)

(transient-define-prefix ymacs-transient/org-download ()
  [("s" "Screenshot" org-download-screenshot)
   ("r" "Rename at point" org-download-rename-at-point)
   ("l" "Rename last" org-download-rename-last)
   ("D" "delete" org-download-delete)])

;;;###autoload
(defun ymacs-org/publish-current-file ()
  (interactive)
  (require 'ox)
  (without-user-record!
   (save-excursion
     (let* ((force (equal current-prefix-arg '(16)))
            (project
             (or (when (not force)
                   (assoc ymacs-org-publish-last-project org-publish-project-alist))
                 (when current-prefix-arg
                   (completing-read! "Project: " org-publish-project-alist))
                 (condition-case nil
                     (unless (yes-or-no-p
                              (format "Use org-export-dispatch(y) or %s (n/RET)?"
                                      (caar org-publish-project-alist)))
                       (car org-publish-project-alist))
                   (quit nil)))))
       (if project
           (save-window-excursion
             (let ((org-publish-use-timestamps-flag
                    (and (not force)
                         org-publish-use-timestamps-flag)))
               (setq-local ymacs-org-publish-last-project (car project))
               (org-publish-file (buffer-file-name (buffer-base-buffer)) project)))
         (org-export-dispatch '(4)))))))

;;;###autoload
(defun ymacs-org/next-item (&optional -n)
  (interactive "p")
  (cond ((org-in-block-p '("src" "quote" "example"))
         (org-next-block (abs -n) (< -n 0)))
        ((org-in-item-p)
         (let ((cmd (if (> -n 0) #'org-next-item #'org-previous-item))
               (col (current-column))
               (n (abs -n)))
           (with-demoted-errors "%s"
             (while (>= (cl-decf n) 0)
               (funcall cmd))
             (move-to-column col))))
        (t
         (let ((cmd (if (> -n 0)
                        #'org-next-visible-heading
                      #'org-previous-visible-heading)))
           (funcall cmd (abs -n))))))

;;;###autoload
(defun ymacs-org/previous-item (&optional -n)
  (interactive "p")
  (ymacs-org/next-item (- -n)))

(defun ymacs-org//auctex-math-all ()
  (append LaTeX-math-list LaTeX-math-default))

(defun ymacs-org//auctex-get-LaTeX-font-list (&optional mathp)
  (delq nil (mapcar
             (lambda (x)
               (and (stringp x)
                    (not (string= x ""))
                    (not (string= x "}"))
                    (list (substring x 1 -1) t)))
             (delete-dups
              (mapcar (if mathp
                          (lambda (x) (nth 3 x))
                        #'cadr)
                      LaTeX-font-list)))))

(defun ymacs-org//auctex-symbol-candidates (prefix)
  (all-completions prefix (append (mapcar 'cadr (ymacs-org//auctex-math-all))
                                  (mapcar 'car (ymacs-org//auctex-get-LaTeX-font-list t)))))

(defun ymacs-org//auctex-symbol-annotation (-candidate)
  (let ((char (nth 2 (assoc -candidate
                            (mapcar 'cdr (ymacs-org//auctex-math-all))))))
    (and char (concat " " (char-to-string (decode-char 'ucs char))))))

;; (defun ymacs-org/company-symbols (-command &optional -arg &rest _)
;;   "Complete math symbol in LaTeX fragments, better than `pcomplete'"
;;   (interactive (list 'interactive))
;;   (cl-case -command
;;     (interactive (company-begin-backend 'ymacs-org/company-symbols))
;;     (prefix
;;      (ignore-errors
;;        (let ((word (company-grab-word)))
;;          (and (= ?\\
;;                  (char-before
;;                   (- (point) (length word))))
;;               word))))
;;     (candidates (ymacs-org//auctex-symbol-candidates -arg))
;;     (annotation (ymacs-org//auctex-symbol-annotation -arg))))

(defsubst ymacs-org//fn-to-extension (-fn)
  (when (symbolp -fn)
    (concat "." (car (last (split-string (symbol-name -fn) "-"))))))

;;;###autoload
(defun ymacs-org/open ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (or (assoc ymacs-org-publish-last-project org-publish-project-alist)
                      (org-publish-get-project-from-filename filename)))
         (basename (file-name-sans-extension filename))
         (files (list
                 (when project
                   (when-let ((src-dir (org-publish-property :base-directory project))
                              (dst-dir (org-publish-property :publishing-directory project)))
                     (concat (expand-file-name (file-relative-name basename src-dir) dst-dir)
                             (or (ymacs-org//fn-to-extension
                                  (org-publish-property :publishing-function project))
                                 ".pdf"))))
                 (concat basename
                         (or (ymacs-org//fn-to-extension
                              (car-safe org-export-dispatch-last-action))
                             ".pdf")))))
    (catch 'done
      (dolist (file files)
        (when (and file (file-exists-p file))
          (ymacs-editor/find-file-externally (list file))
          (throw 'done t)))
      (find-file (read-file-name "File: " nil default-directory nil basename)))))

;;;###autoload
(defun ymacs-org/latexmk-start-watching (-arg)
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
          (user-error "Process latexmk is still active")))
      (start-process
       "latexmk" (get-buffer-create "LaTeXMK")
       "latexmk" "-pvc" "-g" "-pdf" "-dvi-" "-ps-"
       "-pdflatex=xelatex -shell-escape -interaction=nonstopmode %O %S"
       (concat "-outdir=" out-dir)
       (concat base-name ".tex"))
      (message "latexmk started ..."))))

;;;###autoload
(defun ymacs-org/split-src-block (&optional -below)
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
(defun ymacs-org/delete-latex-fragement-cache ()
  (interactive)

  (let ((image-dir (expand-file-name org-preview-latex-image-directory)))
    (when (yes-or-no-p (format "Delete images in %s" image-dir))
      (dolist (image-file
               (directory-files image-dir :full "\\.\\(png\\|jpe?g\\|svg\\)\\'"))
        (with-demoted-errors "Error: %s"
          (delete-file image-file))))))


(defun ymacs-org//create-preview-overlay (-latex-fragment)
  (unless (or (string-prefix-p "\\begin" -latex-fragment)
              (string-prefix-p "$" -latex-fragment))
    (setq -latex-fragment
          (concat "\\begin{displaymath}"
                  -latex-fragment
                  "\\end{displaymath}")))

  (with-current-buffer (get-buffer-create "*latex-preview*")
    (setq buffer-read-only nil)
    (setq-local mode-line-format nil)

    (erase-buffer)
    (insert -latex-fragment)

    (unless (eq major-mode 'org-mode)
      (org-mode))

    (org-clear-latex-preview)

    (let ((org-format-latex-options
           (plist-put (copy-sequence org-format-latex-options) :scale 2)))
      (org--latex-preview-region (point-min) (point-max)))

    (let ((overlays (cl-remove-if-not
                     (lambda (ov) (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
                     (overlays-in (point-min) (point-max)))))
      (cl-assert (= (length overlays) 1) nil "Too many/few overlays were created")
      (car overlays))))

;;;###autoload
(defun ymacs-org/display-latex-fragment-at-point ()
  (interactive)
  (if (org-clear-latex-preview (point) (1+ (point)))
      (message "LaTeX preview removed")
    (when-let*
        ((bounds
          (or (and (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
              (when-let ((datum (org-element-context)))
                (and (memq (org-element-type datum) '(latex-environment latex-fragment))
                     (cons (org-element-property :begin datum)
                           (org-element-property :end datum))))
              (save-mark-and-excursion
                (let ((current-point (point))
                      beg end)
                  (forward-paragraph -1)
                  (setq beg (point))
                  (when (re-search-forward
                         (rx line-start ".." (* space) "math::" (* space))
                         current-point t 1)
                    (setq beg (point)))
                  (forward-paragraph 1)
                  (skip-chars-backward "\n\t ")
                  (setq end (point))
                  (cons beg end)))))
         (latex-fragment
          (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))
                       (rx (* (any blank "\n,.!?;:")))
                       (rx (* (any blank "\n,.!?;:")))))
         (ov (ymacs-org//create-preview-overlay latex-fragment)))
      (move-overlay ov
                    (car bounds) (cdr bounds)
                    (current-buffer)))))

;;;###autoload
(defun ymacs-org/project-open ()
  (interactive)
  (let* ((src-dir (file-truename ymacs-org-project-src-dir))
         (prefix-length (length (file-name-as-directory src-dir)))
         (name (completing-read "Open note: "
                                (--map
                                 (substring it prefix-length)
                                 (directory-files-recursively src-dir "\\.org\\'"))
                                nil nil nil
                                'org-project-note-history)))
    (setq name (abbreviate-file-name (expand-file-name name ymacs-org-project-src-dir)))
    (cond ((not (string-suffix-p ".org" name))
           (user-error "filename should endswith \".org\""))
          ((file-exists-p name)
           (message "Open existing note: %s" name)
           (find-file name))
          ((y-or-n-p (format "Create new note %s" (abbreviate-file-name name)))
           (make-directory (file-name-directory name) t)
           (find-file name))
          (t
           (user-error "Nothing to do with %s" name)))))
