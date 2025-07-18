;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'python))

(defun ymacs-python//get-or-create-process (-directory -dedicated)
  (let* ((default-directory -directory)
         (name (python-shell-get-process-name -dedicated))
         (buffer (or
                  (get-buffer (format "*%s*" name))
                  (python-shell-make-comint (python-shell-calculate-command) name t)))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (let ((cumtime 0))
        (while (and (not (bound-and-true-p python-shell--first-prompt-received))
                    (< cumtime 3))
          (when (> cumtime 0.5)
            (message "Wait for first prompt ...(%.1f)" cumtime))
          (sleep-for 0.1)
          (setq cumtime (+ cumtime 0.1)))))
    process))

(defun ymacs-python//send-region-or-buffer (&optional -send-main)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end) -send-main)

    (python-shell-send-buffer -send-main)
    (when (unless -send-main
            (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:"))
              (save-excursion
                (goto-char (point-min))
                (re-search-forward if-main-regex nil t))))
      (message (concat "Removed if __name__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))

(defun ymacs-python//enable-pdbtrack ()
  (unless (memq 'python-pdbtrack-comint-input-filter-function
                comint-output-filter-functions)
    (python-pdbtrack-setup-tracking))

  (add-to-list 'mode-line-buffer-identification
               (propertize "[pdbtrack]" 'face 'font-lock-builtin-face)
               t)

  (add-hook 'comint-output-filter-functions
            #'python-pdbtrack-comint-output-filter-function
            nil t)

  (force-mode-line-update))

(defun ymacs-python//disable-pdbtrack ()
  (remove-hook 'mode-line-buffer-identification
               "[pdbtrack]")
  (remove-hook 'comint-output-filter-functions
               #'python-pdbtrack-comint-output-filter-function
               t)
  (force-mode-line-update))

(defun ymacs-python//shell-running-p ()
  (when-let* ((process (or (and (derived-mode-p 'inferior-python-mode)
                                (get-buffer-process (current-buffer)))
                           (python-shell-get-process))))
    (with-current-buffer (process-buffer process)
      (not comint-last-prompt))))

(defsubst ymacs-python//triple-quotes-p (-start -end)
  (let ((str (buffer-substring-no-properties
              (max -start (point-min))
              (min -end (point-max)))))
    (and (or (string= str "'''")
             (string= str "\"\"\""))
         str)))

(defun ymacs-python//around-string-start+end-points (-fn &optional -state)
  (when-let ((start+end (funcall -fn -state))
             (start (car start+end))
             (end (cdr start+end)))
    (if (ymacs-python//triple-quotes-p (- end 2) (+ end 1))
        (cons (+ start 2) (- end 2))
      start+end)))

(defsubst ymacs-python//line-startswith-p (-string)
  (back-to-indentation)
  (string-prefix-p -string (buffer-substring-no-properties (point) (line-end-position))))

(defsubst ymacs-python//get-execution-root-from-pyright-config ()
  (let* ((project-root (ymacs-editor//project-root))
         (config-file (expand-file-name "pyrightconfig.json" project-root)))
    (when (file-exists-p config-file)
      (let ((config (with-temp-buffer
                      (insert-file-contents config-file)
                      (goto-char (point-min))
                      (json-parse-buffer))))
        (cl-loop
         for env across (gethash "executionEnvironments" config)
         for root = (and (hash-table-p env)
                         (gethash "root" env))
         for root-dir = (and root
                             (expand-file-name root project-root))
         when (and root-dir
                   (buffer-file-name)
                   (file-in-directory-p (buffer-file-name) root-dir))
         return root-dir)))))

(defsubst ymacs-python//get-execution-root ()
  (if (not (eq ymacs-python-execution-root 'unset))
      ymacs-python-execution-root
    (setq ymacs-python-execution-root
          (or (ymacs-python//get-execution-root-from-pyright-config)
              (ymacs-editor//project-root-or-default)))))

(defun ymacs-python//get-venv-source-cmd ()
  (when (and ymacs-python-auto-activate-venv-p python-shell-virtualenv-root)
    (if (and (stringp python-shell-virtualenv-root)
             (if-let ((remote-host (file-remote-p default-directory)))
                 (file-directory-p (concat remote-host python-shell-virtualenv-root))
               (file-directory-p python-shell-virtualenv-root)))
        (format "source %s\n" (expand-file-name "bin/activate" python-shell-virtualenv-root))
      (user-error "virtualenv %s doesn't exists" python-shell-virtualenv-root))))
