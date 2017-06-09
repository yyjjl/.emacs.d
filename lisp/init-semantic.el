(with-eval-after-load 'semantic
  (semantic-add-system-include "/usr/include/c++/5/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c-mode)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          ;; global-semantic-idle-summary-mode
          ;; global-semantic-idle-local-symbol-highlight-mode
          global-semantic-stickyfunc-mode
          ;; error occurs a lot
          ;; global-semantic-decoration-mode
          global-semantic-highlight-func-mode
          global-semantic-mru-bookmark-mode))
  (require 'stickyfunc-enhance))

(with-eval-after-load "db-file"
  ;; Fix remote file problem
  (defmethod semanticdb-live-p ((obj semanticdb-project-database))
    "Return non-nil if the file associated with OBJ is live.
  Live databases are objects associated with existing directories."
    (and (slot-boundp obj 'reference-directory)
         (let ((dir (oref obj reference-directory)))
           (and (not (file-remote-p dir)) (file-exists-p dir))))))

(defun try-turn-on-semantic-mode ()
  (if (> (buffer-size) large-buffer-size)
      (semantic-mode -1)
    (semantic-mode 1)))

(provide 'init-semantic)
