;;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

;;;###autoload
(defun ymacs-default/generate-autoloads ()
  (interactive)
  (require 'autoload)

  (let ((pattern "\\`commands\\(-[^\\.]*\\)?\\.el\\'")
        (success-count 0)
        (failed-count 0))
    (with-current-buffer (find-file-noselect ymacs-autoloads-file)
      (erase-buffer)
      (cl-loop
       for (_ . feature) in ymacs-loaded-features
       for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
       when (file-directory-p directory)
       do (dolist (file (directory-files directory t pattern))
            (condition-case nil
                (progn
                  (generate-file-autoloads file)
                  (cl-incf success-count))
              (user-error
               (cl-incf failed-count)))))
      (save-buffer))
    (print! "> Autoload for %d files generated, %d files failed\n" success-count failed-count)

    (when (called-interactively-p 'interactive)
      (load ymacs-autoloads-file))))

(defvar ymacs-emacs-version-stamp-file
  (expand-file-name "emacs-version" ymacs-cache-directory)
  "File recording the Emacs version that last compiled ELPA packages.")

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
different Emacs than the running one.  VERSION is nil when unknown."
  (let (stale)
    (dolist (elc (directory-files-recursively -directory "\\.elc\\'"))
      (let ((version (ymacs-default//elc-compiled-version elc)))
        (unless (equal version emacs-version)
          (push (cons elc version) stale))))
    (nreverse stale)))

;;;###autoload
(defun ymacs-default/check-stale-elc (&optional -purge)
  "Warn about ELPA `.elc' files not compiled by the running Emacs.

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
        (message "[ymacs] All ELPA .elc match Emacs %s" emacs-version)))
     (-purge
      (dolist (it stale)
        (ignore-errors (delete-file (car it))))
      (message "[ymacs] Deleted %d stale .elc (not compiled by Emacs %s); run `make config' to recompile"
               (length stale) emacs-version))
     (t
      (message "[ymacs] %d ELPA .elc were compiled by another Emacs (running %s); run `make config' to recompile"
               (length stale) emacs-version)))
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
    ;; When Emacs itself changed, timestamp-based recompilation is not enough:
    ;; the source is not newer than the .elc, yet macro expansions may differ.
    ;; Purge every stale .elc first so the recompile below rebuilds them.
    (unless (equal last-version emacs-version)
      (let ((stale (ymacs-default//stale-elc-files package-user-dir)))
        (print! "> Emacs %s -> %s: purging %d stale .elc in %s\n"
                (or last-version "?") emacs-version (length stale)
                (abbreviate-file-name package-user-dir))
        (dolist (it stale)
          (ignore-errors (delete-file (car it))))))
    (byte-recompile-directory package-user-dir 0)
    (with-temp-file ymacs-emacs-version-stamp-file
      (insert emacs-version))))

;;;###autoload
(defun ymacs-default/compile-config ()
  (interactive)
  (dolist (file (cl-remove-duplicates
                 (cl-remove-if
                  (lambda (file-name)
                    (or (string-suffix-p "-test.el" file-name)
                        (string-suffix-p "-pkg.el" file-name)
                        (string-suffix-p "elpa.el" file-name)
                        (string-suffix-p ".dir-locals.el" file-name)))
                  (append
                   (when (file-exists-p ymacs-site-lisp-directory)
                     (cl-loop
                      for dir in (directory-files ymacs-site-lisp-directory)
                      when (not (string-match-p "^\\." dir))
                      append (directory-files (expand-file-name dir ymacs-site-lisp-directory) t "\\.el$")))
                   (directory-files ymacs-config-directory t "\\.el$")
                   (directory-files user-emacs-directory t "\\.el$")
                   (cl-loop
                    for (_ . feature) in ymacs-loaded-features
                    for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
                    nconc (directory-files-recursively directory "\\.el$"))))))
    (condition-case err
        (progn
          (print! "> Compiling %s\n" file)
          (byte-compile-file file)
          (when (native-comp-available-p)
            (native-compile file)))
      (error
       (print! "  > Failed %s\n" err)))))
