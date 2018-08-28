(with-eval-after-load 'semantic
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)

  (advice-add 'semantic-new-buffer-fcn :around 'ignore-errors!)
  (advice-add 'semantic-idle-scheduler-function
              :around (lambda (-fn &rest -args)
                        (with-local-quit (apply -fn -args))))

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
  (setq semantic-idle-scheduler-idle-time 1)

  (defun core/semantic-new-buffer-fcn-hack (-fn)
    (unless (and default-directory (file-remote-p default-directory))
      (funcall -fn)))

  (advice-add 'semantic-new-buffer-fcn
              :around #'core/semantic-new-buffer-fcn-hack)

  (when env-has-gtags-p
    (dolist (mode '(c++-mode c-mode java-mode))
      (semanticdb-enable-gnu-global-databases mode))))

(defun core//semanticdb-parse-directory (-dir &optional -regex -recursive-p)
  (without-user-record!
   (let ((dir (file-name-as-directory -dir)))
     (dolist (file (directory-files dir))
       (unless (or (member file '("." ".."))
                   (member file core-ignored-directories))
         (let ((full-file (expand-file-name file dir)))
           (condition-case err
               (if (and -recursive-p (file-directory-p full-file))
                   (core//semanticdb-parse-directory full-file -regex -recursive-p)
                 (if (or (not -regex)
                         (string= -regex "")
                         (string-match-p -regex file))
                     (save-excursion
                       (semanticdb-file-table-object full-file))))
             (error (message "Error: %s" err)))))))))

(defun core/semantidb-parse (-regex -dir)
  (interactive (list (read-regexp "File name regex (default nil)" nil)
                     (file-name-directory
                      (completing-read "Directory: "
                                       #'read-file-name-internal))))
  (unwind-protect
      (progn
        (core//semanticdb-parse-directory -dir -regex t)
        (semanticdb-cleanup-cache-files))
    (let ((inhibit-quit t))
      (semanticdb-save-all-db))
    (when-let (buffer (get-buffer "*Semanticdb Delete*"))
      (kill-buffer buffer))))

(defun core/enable-semantic ()
  (semantic-mode 1)
  ;; (global-semantic-show-parser-state-mode 1)
  ;; (global-semantic-highlight-edits-mode 1)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))

(add-hook 'after-init-hook 'core/enable-semantic)

(provide 'core-semantic)
