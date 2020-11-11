;;; -*- lexical-binding: t; -*-

(defun ymacs-org//hot-expand (-type)
  "Expand org template."
  (org-insert-structure-template -type))

(defhydra ymacs-hydra/org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _E_macs-lisp    _L_aTeX:
_l_atex   _e_xample   _C_pp           _i_ndex:
_a_scii   _v_erse     _I_NCLUDE:      _j_avascript
_s_rc     _S_hell     ^ ^             _H_TML:
_h_tml    _'_         ^ ^             _A_SCII:
"
  ("s" (yas-expand-snippet "#+begin_src $1\n$0\n#+end_src"))
  ("e" (ymacs-org//hot-expand "example"))
  ("q" (ymacs-org//hot-expand "quote"))
  ("v" (ymacs-org//hot-expand "verse"))
  ("c" (ymacs-org//hot-expand "center"))
  ("l" (ymacs-org//hot-expand "latex"))
  ("h" (ymacs-org//hot-expand "html"))
  ("a" (ymacs-org//hot-expand "ascii"))
  ("i" (ymacs-org//hot-expand "index"))
  ("E" (ymacs-org//hot-expand "src "))
  ("C" (yas-expand-snippet "#+begin_src cpp\n$0\n#+end_src"))
  ("S" (yas-expand-snippet "#+begin_src sh\n$0\n#+end_src"))
  ("j" (yas-expand-snippet "#+begin_src javascript\n$0\n#+end_src"))
  ("L" (ymacs-org//hot-expand "export latex"))
  ("H" (ymacs-org//hot-expand "export html"))
  ("A" (ymacs-org//hot-expand "export ascii"))
  ("I" (yas-expand-snippet "#+include: $0"))
  ("'" (yas-expand-snippet "#+begin_${1:type}\n$0\n#+end_$1"))
  ("<" self-insert-command "self-insert"))

;;;###autoload
(defun ymacs-org/hot-expand ()
  (interactive)
  (if (looking-back "^\\s-*" (line-beginning-position))
      (ymacs-hydra/org-template/body)
    (self-insert-command 1)))

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
                   (ymacs//completing-read-simple "Project: " org-publish-project-alist))
                 (car org-publish-project-alist))))
       (save-window-excursion
         (let ((org-publish-use-timestamps-flag
                (and (not force)
                     org-publish-use-timestamps-flag)))
           (org-publish-file (buffer-file-name (buffer-base-buffer)) project)
           (setq ymacs-org-publish-last-project (car project))))))))

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

;;;###autoload
(defun ymacs-org/company-symbols (-command &optional -arg &rest _)
  "Complete math symbol in LaTeX fragments, better than `pcomplete'"
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'ymacs-org/company-symbols))
    (prefix
     (ignore-errors
       (let ((word (company-grab-word)))
         (and (= ?\\
                 (char-before
                  (- (point) (length word))))
              word))))
    (candidates (company-auctex-symbol-candidates -arg))
    (annotation (company-auctex-symbol-annotation -arg))))

(defsubst ymacs-org//fn-to-extension (-fn)
  (when (symbolp -fn)
    (concat "." (car (last (split-string (symbol-name -fn) "-"))))))

;;;###autoload
(defun ymacs-org/open-pdf ()
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
          (if (fboundp #'ymacs-pdf//find-file-other-window)
              (ymacs-pdf//find-file-other-window file)
            (open! (list file)))
          (throw 'done t)))
      (counsel-find-file basename))))

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
       "latexmk" "-quiet" "-pvc" "-g" "-pdf" "-dvi-" "-ps-"
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
(defun ymacs-org/create-latex-fragemnt-cache (-files -clean-invalid-p)
  (interactive
   (list
    (if (and (not current-prefix-arg)
             (string-suffix-p ".org" (buffer-file-name)))
        ;; create for current file
        (list (buffer-file-name))
      (let ((dir-or-file-name (read-file-name "File or Directory: ")))
        (cond
         ((file-directory-p dir-or-file-name)
          (directory-files-recursively dir-or-file-name "\\.org\\'"))
         ((file-exists-p dir-or-file-name)
          (list dir-or-file-name))
         (t (user-error "%s is neither a directory nor file" dir-or-file-name)))))
    (and current-prefix-arg (yes-or-no-p "Clean invalid cache?"))))

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
               (org-clear-latex-preview)
               (org-format-latex prefix
                                 (point-min) (point-max)
                                 default-directory
                                 'overlays
                                 (concat "The %s for `" file "'")
                                 'forbuffer
                                 org-preview-latex-default-process)
               (dolist (ov (overlays-in (point-min) (point-max)))
                 (-when-let (image-file
                             (and (eq (overlay-get ov 'org-overlay-type)
                                      'org-latex-overlay)
                                  (plist-get (cdr-safe (overlay-get ov 'display)) :file)))
                   (puthash (expand-file-name image-file
                                              org-preview-latex-image-directory)
                            t
                            image-files)))))))))
    (if -clean-invalid-p
        (let ((count 0))
          (cl-loop for image-dir being the hash-keys of image-dirs
                   when (file-exists-p image-dir)
                   do (dolist (image-file
                               (directory-files image-dir :full "\\.\\(png\\|jpe?g\\|svg\\)\\'"))
                        (unless (gethash image-file image-files)
                          (cl-incf count)
                          (ignore-errors (delete-file image-file)))))
          (message "Deleted %d invalid cache files" count))
      (message "Invalid cache files were not checked"))))

;;;###autoload
(defun ymacs-org/display-latex-fragment-at-point ()
  (interactive)
  (if-let ((latex-fragment
            (when-let ((bounds (if (region-active-p)
                                   (cons (region-beginning)
                                         (region-end))
                                 (when-let (element (org-element-at-point))
                                   (cons (org-element-property :begin element)
                                         (org-element-property :end element))))))
              (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))
                           (rx (* (any blank "\n,.!?;:")))
                           (rx (* (any blank "\n,.!?;:"))))))
           (buffer (get-buffer-create "*latex-preview*")))
      (progn
        (unless (or (string-prefix-p "\\begin" latex-fragment)
                    (string-prefix-p "$" latex-fragment))
          (setq latex-fragment
                (concat "\\begin{displaymath}"
                        latex-fragment
                        "\\end{displaymath}")))

        (ymacs-popup//display-buffer buffer nil '(:side below :autoclose t :size 0.3))

        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (setq-local mode-line-format nil)

          (erase-buffer)
          (insert latex-fragment)

          (unless (eq major-mode 'org-mode)
            (org-mode))

          (org-clear-latex-preview)
          (let ((org-format-latex-options
                 (plist-put (copy-sequence org-format-latex-options) :scale 2)))
            (org-latex-preview '(16)))

          (when-let (window (get-buffer-window buffer))
            (fit-window-to-buffer window 15)
            (ymacs-popup//push-window window buffer t)
            (special-mode))))
    (user-error "Unknown error")))

;;;###autoload
(defun ymacs-org/project-open ()
  (interactive)
  (let* ((src-dir (file-truename ymacs-org-project-src-dir))
         (prefix-length (length (file-name-as-directory src-dir)))
         (name (ivy-read "Open note: "
                         (--map
                          (substring it prefix-length)
                          (directory-files-recursively src-dir "\\.org\\'"))
                         :history 'org-project-note-history
                         :caller 'org/project-open)))
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

;;;###autoload
(defun ymacs-org/project-sync ()
  (interactive)
  (unless ymacs-org-project-sync-command
    (user-error "Variable `ymacs-org-project-sync-command' is not set."))
  (let ((default-directory ymacs-org-project-base-dir))
    (compilation-start ymacs-org-project-sync-command)))
