(with-eval-after-load 'semantic
  (semantic-add-system-include "/usr/include/c++/5/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  ;; It's too slow, when file is large
  ;; (require 'stickyfunc-enhance)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          global-semantic-stickyfunc-mode
          ;; Error occurs a lot
          ;; global-semantic-decoration-mode
          global-semantic-highlight-func-mode
          global-semantic-mru-bookmark-mode)))

(with-eval-after-load "db-file"
  (when (< emacs-major-version 25)
    (fset 'cl-defmethod 'defmethod))
  ;; Fix remote file problem
  (cl-defmethod semanticdb-live-p ((obj semanticdb-project-database))
    "Return non-nil if the file associated with OBJ is live.
  Live databases are objects associated with existing directories."
    (and (slot-boundp obj 'reference-directory)
         (let ((dir (oref obj reference-directory)))
           (and (not (file-remote-p dir)) (file-exists-p dir))))))

(defun main|semanticdb-parse-directory (dir &optional regex recursive-p)
  (let ((dir (file-name-as-directory dir)))
    (dolist (file (directory-files dir))
      (unless (or (member file '("." ".."))
                  (member file core|ignored-directories))
        (let ((full-file (expand-file-name file dir)))
          (condition-case err
              (if (and (file-directory-p full-file) recursive-p)
                  (main|semanticdb-parse-directory full-file regex recursive-p)
                (if (or (not regex)
                        (string= regex "")
                        (string-match-p regex file))
                    (save-excursion
                      (semanticdb-file-table-object full-file))))
            (error (message "Error: %s" err))))))))

(defun main|semantidb-parse (regex dir)
  (interactive (list (read-regexp "File name regex (default nil)" nil)
                     (file-name-directory
                      (completing-read "Directory: " #'read-file-name-internal))))
  (main|semanticdb-parse-directory dir regex t)
  (semanticdb-save-all-db))

(defun main|semantic-mode ()
  (if (or (> (buffer-size) core|large-buffer-size)
          (file-remote-p default-directory))
      (semantic-mode -1)
    (semantic-mode 1)))

(provide 'init-semantic)
