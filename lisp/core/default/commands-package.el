;;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

(defun ymacs-default//autoload-source-directories ()
  "Return directories that can contribute generated autoloads."
  (sort
   (cl-remove-duplicates
    (cl-loop
     for (_ . feature) in ymacs-loaded-features
     for directory = (expand-file-name (symbol-name feature)
                                       ymacs-config-directory)
     when (file-directory-p directory)
     collect directory)
    :test #'file-equal-p)
   #'string<))

(defun ymacs-default//autoload-source-files ()
  "Return the command files that contribute to the generated autoloads."
  (let ((pattern "\\`commands\\(-[^\\.]*\\)?\\.el\\'"))
    (sort
     (cl-loop
      for directory in (ymacs-default//autoload-source-directories)
      nconc (directory-files directory t pattern))
     #'string<)))

(defun ymacs-default//autoload-source-manifest ()
  "Return stable metadata for every generated-autoload source file."
  (mapcar
   (lambda (file)
     (let ((attributes (file-attributes file 'string)))
       (list (file-relative-name file user-emacs-directory)
             (file-attribute-size attributes)
             (file-attribute-modification-time attributes))))
   (ymacs-default//autoload-source-files)))

;;;###autoload
(defun ymacs-default/autoloads-stale-p ()
  "Return non-nil when generated autoloads do not match their sources."
  (or (not (file-exists-p ymacs-autoloads-file))
      (not (file-exists-p ymacs-autoloads-manifest-file))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents ymacs-autoloads-manifest-file)
            (not (equal (read (current-buffer))
                        (ymacs-default//autoload-source-manifest))))
        (error t))))

;;;###autoload
(defun ymacs-default/generate-autoloads ()
  (interactive)
  (require 'loaddefs-gen)

  (let* ((directories (ymacs-default//autoload-source-directories))
         (files (ymacs-default//autoload-source-files))
         (manifest (ymacs-default//autoload-source-manifest))
         (excluded-files
          (cl-loop for directory in directories
                   nconc (cl-remove-if
                          (lambda (file)
                            (cl-member file files :test #'file-equal-p))
                          (directory-files directory t "\\.el\\'")))))
    (loaddefs-generate directories ymacs-autoloads-file excluded-files
                       ";;; -*- lexical-binding: t; -*-\n" nil t)
    (with-temp-file ymacs-autoloads-manifest-file
      (prin1 manifest (current-buffer))
      (insert "\n"))
    (print! "> Autoload for %d files generated\n" (length files))

    (when (called-interactively-p 'interactive)
      (load ymacs-autoloads-file))))

(defvar ymacs-emacs-version-stamp-file
  (expand-file-name "emacs-version" ymacs-cache-directory)
  "File recording the Emacs major version that last compiled ELPA packages.")

(defsubst ymacs-default//emacs-major-version-string (-version)
  "Return the major component of Emacs version string -VERSION."
  (when (and -version (string-match "\\`[0-9]+" -version))
    (match-string 0 -version)))

(defun ymacs-default//elc-compiled-version (-elc-file)
  "Return the Emacs version string that compiled -ELC-FILE, or nil.

Reads only the header of -ELC-FILE, which looks like

  ;;; Compiled
  ;;; in Emacs version 31.1
  ;;; with all optimizations."
  (with-temp-buffer
    (ignore-errors
      (insert-file-contents-literally -elc-file nil 0 256))
    (goto-char (point-min))
    (when (re-search-forward "in Emacs version \\([0-9.]+\\)" nil t)
      (match-string 1))))

(defun ymacs-default//stale-elc-files (-directory)
  "Return a list of (FILE . VERSION) under -DIRECTORY compiled by a
different Emacs major version.  VERSION is nil when unknown."
  (let (stale)
    (dolist (elc (directory-files-recursively -directory "\\.elc\\'"))
      (let ((version (ymacs-default//elc-compiled-version elc)))
        (unless (equal (ymacs-default//emacs-major-version-string version)
                       (number-to-string emacs-major-version))
          (push (cons elc version) stale))))
    (nreverse stale)))

;;;###autoload
(defun ymacs-default/check-stale-elc (&optional -purge)
  "Warn about ELPA `.elc' files compiled by another Emacs major version.

`byte-recompile-directory' only rebuilds when the source is newer than
the byte-code, so upgrading Emacs alone never invalidates ELPA `.elc'.
Loading such stale byte-code can raise errors like `void-variable' when
a macro (e.g. `define-globalized-minor-mode') expands differently across
versions.

With -PURGE non-nil (or a prefix argument), delete the stale files so
they are recompiled on the next `make config'.  Returns the list of
stale (FILE . VERSION) pairs."
  (interactive "P")
  (let ((stale (ymacs-default//stale-elc-files package-user-dir)))
    (cond
     ((null stale)
      (when (called-interactively-p 'interactive)
        (message "[ymacs] All ELPA .elc match Emacs major version %s"
                 emacs-major-version)))
     (-purge
      (dolist (it stale)
        (ignore-errors (delete-file (car it))))
      (message "[ymacs] Deleted %d stale .elc (not compiled by Emacs %s.x); run `make config' to recompile"
               (length stale) emacs-major-version))
     (t
      (message "[ymacs] %d ELPA .elc were compiled by another Emacs major version (running %s.x); run `make config' to recompile"
               (length stale) emacs-major-version)))
    stale))

;;;###autoload
(defun ymacs-default/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let* ((inhibit-message -no-message)
         (last-version
          (when (file-exists-p ymacs-emacs-version-stamp-file)
            (with-temp-buffer
              (insert-file-contents ymacs-emacs-version-stamp-file)
              (string-trim (buffer-string))))))
    ;; When the Emacs major version changed, timestamp-based recompilation is not enough:
    ;; the source is not newer than the .elc, yet macro expansions may differ.
    ;; Purge every stale .elc first so the recompile below rebuilds them.
    (unless (equal (ymacs-default//emacs-major-version-string last-version)
                   (number-to-string emacs-major-version))
      (let ((stale (ymacs-default//stale-elc-files package-user-dir)))
        (print! "> Emacs %s -> %s.x: purging %d stale .elc in %s\n"
                (or last-version "?") emacs-major-version (length stale)
                (abbreviate-file-name package-user-dir))
        (dolist (it stale)
          (delete-file (car it)))))
    (let ((failed-files nil)
          (original-byte-compile-file (symbol-function 'byte-compile-file)))
      (cl-letf (((symbol-function 'byte-compile-file)
                 (lambda (file &optional load)
                   (let ((result (funcall original-byte-compile-file file load)))
                     (unless result
                       (push file failed-files))
                     result))))
        (byte-recompile-directory package-user-dir 0))
      (when failed-files
        (error "Failed to compile %d ELPA files: %s"
               (length failed-files)
               (mapconcat #'identity (nreverse failed-files) ", "))))
    (with-temp-file ymacs-emacs-version-stamp-file
      (insert (number-to-string emacs-major-version)))))

;;;###autoload
(defun ymacs-default/compile-config ()
  (interactive)
  (let ((files
         (cl-remove-duplicates
          (cl-remove-if
           (lambda (file-name)
             (or (string-suffix-p "-test.el" file-name)
                 (string-suffix-p "-pkg.el" file-name)
                 (string-suffix-p "elpa.el" file-name)
                 (string-suffix-p ".dir-locals.el" file-name)))
           (append
            (when (file-directory-p ymacs-site-lisp-directory)
              (cl-loop
               for name in (directory-files ymacs-site-lisp-directory)
               for directory = (expand-file-name name ymacs-site-lisp-directory)
               when (and (not (string-prefix-p "." name))
                         (file-directory-p directory))
               append (directory-files directory t "\\.el$")))
            (directory-files ymacs-config-directory t "\\.el$")
            (directory-files user-emacs-directory t "\\.el$")
            (cl-loop
             for (_ . feature) in ymacs-loaded-features
             for directory = (expand-file-name (symbol-name feature)
                                               ymacs-config-directory)
             when (file-directory-p directory)
             nconc (directory-files-recursively directory "\\.el$"))))))
        failures)
    (dolist (file files)
      (condition-case err
          (progn
            (print! "> Compiling %s\n" file)
            (unless (byte-compile-file file)
              (error "Byte compilation returned nil"))
            (when (native-comp-available-p)
              (native-compile file)))
        (error
         (push (cons file (error-message-string err)) failures)
         (print! "  > Failed %s: %s\n" file (error-message-string err)))))
    (when failures
      (error "Failed to compile %d config files" (length failures)))
    t))
