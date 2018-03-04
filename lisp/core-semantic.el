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
          ;; global-semantic-highlight-func-mode
          global-semantic-mru-bookmark-mode))
  (add-to-list 'semantic-inhibit-functions
               (lambda () (file-remote-p default-directory)))

  (when tags-has-gtags-p
    (dolist (mode '(c++-mode c-mode java-mode))
      (semanticdb-enable-gnu-global-databases mode))))

(defun core/semanticdb-parse-directory ($dir &optional $regex $recursive?)
  (let ((dir (file-name-as-directory $dir)))
    (dolist (file (directory-files dir))
      (unless (or (member file '("." ".."))
                  (member file core-ignored-directories))
        (let ((full-file (expand-file-name file dir)))
          (condition-case err
              (if (and (file-directory-p full-file) $recursive?)
                  (core/semanticdb-parse-directory full-file
                                                   $regex $recursive?)
                (if (or (not $regex)
                        (string= $regex "")
                        (string-match-p $regex file))
                    (save-excursion
                      (semanticdb-file-table-object full-file))))
            (error (message "Error: %s" err))))))))

(defun core/semantidb-parse ($regex $dir)
  (interactive (list (read-regexp "File name regex (default nil)" nil)
                     (file-name-directory
                      (completing-read "Directory: "
                                       #'read-file-name-internal))))
  (core/semanticdb-parse-directory $dir $regex t)
  (semanticdb-save-all-db))

(defun core/enable-semantic ()
  (semantic-mode 1)
  (core/toggle-semantic-completion :no-message))

(defun core/toggle-semantic-completion (&optional $no-message-p)
  (interactive)
  (if (--some (string-prefix-p "semantic-" (symbol-name it))
              (default-value 'completion-at-point-functions))
      (progn
        (unless $no-message-p
          (message "Remove semantic-* from `completion-at-point-functions'"))
        (remove-hook 'completion-at-point-functions
                     'semantic-analyze-completion-at-point-function)
        (remove-hook 'completion-at-point-functions
                     'semantic-analyze-notc-completion-at-point-function)
        (remove-hook 'completion-at-point-functions
                     'semantic-analyze-nolongprefix-completion-at-point-function))
    (unless $no-message-p
      (message "Add semantic-* to `completion-at-point-functions'"))
    (add-hook 'completion-at-point-functions
              'semantic-analyze-nolongprefix-completion-at-point-function)
    (add-hook 'completion-at-point-functions
              'semantic-analyze-notc-completion-at-point-function)
    (add-hook 'completion-at-point-functions
              'semantic-analyze-completion-at-point-function)))

(provide 'core-semantic)
