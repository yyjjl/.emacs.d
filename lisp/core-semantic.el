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
          ;; global-semantic-mru-bookmark-mode
          ))
  (add-to-list 'semantic-inhibit-functions
               (lambda () (file-remote-p default-directory))))

(defun core/semanticdb-parse-directory ($dir &optional $regex $recursive?)
  (let ((dir (file-name-as-directory $dir)))
    (dolist (file (directory-files dir))
      (unless (or (member file '("." ".."))
                  (member file core-ignored-directories))
        (let ((full-file (expand-file-name file dir)))
          (condition-case err
              (if (and (file-directory-p full-file) $recursive?)
                  (core/semanticdb-parse-directory full-file $regex $recursive?)
                (if (or (not $regex)
                        (string= $regex "")
                        (string-match-p $regex file))
                    (save-excursion
                      (semanticdb-file-table-object full-file))))
            (error (message "Error: %s" err))))))))

(defun core/semantidb-parse ($regex $dir)
  (interactive (list (read-regexp "File name regex (default nil)" nil)
                     (file-name-directory
                      (completing-read "Directory: " #'read-file-name-internal))))
  (core/semanticdb-parse-directory $dir $regex t)
  (semanticdb-save-all-db))

(provide 'core-semantic)
