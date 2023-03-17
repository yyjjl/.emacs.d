;;; -*- lexical-binding: t; -*-

(after! pyvenv
  (define-advice pyvenv-track-virtualenv (:around (-fn) deactivate)
    (if (and (stringp pyvenv-activate)
             (string-empty-p pyvenv-activate))
        (pyvenv-deactivate)
      (funcall -fn)))

  (define-hook! ymacs-python//shell-exec-hook (ymacs-term-shell-exec-hook)
    (when ymacs-python-auto-activate-venv-p
      (let ((venv (getenv "VIRTUAL_ENV")))
        (when (and (stringp venv)
                   (file-directory-p venv))
          (ymacs-term//send-string
           (format "source %s\n"
                   (expand-file-name "bin/activate" venv))))))))

(after! cython-mode
  (define-hook! ymacs-cython//setup (cython-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (local-set-key (kbd "C-c C-b") nil)))

(after! python
  (define-hook! ymacs-python//compilation-setup (comint-exec-hook)
    (when (bound-and-true-p compilation-shell-minor-mode)
      (when (cl-some (lambda (str)
                       (let ((exe (car (split-string str))))
                         (and (string-match-p "python" exe)
                              (executable-find exe))))
                     (process-command (get-buffer-process (current-buffer))))
        (setq truncate-lines nil)
        (ymacs-python//enable-pdbtrack))))

  (define-hook! ymacs-python//setup (python-mode-hook python-ts-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (when (file-remote-p default-directory)
      (setq-local python-shell-interpreter "python3")
      (setq-local python-shell-interpreter-args "-i"))

    ;; too slow
    (remove-hook 'completion-at-point-functions 'python-completion-at-point t)
    (remove-hook 'eldoc-documentation-functions 'python-eldoc-function t)
    (remove-hook 'flymake-diagnostic-functions 'python-flymake t)

    (eval-when-has-feature! lsp
      (with-transient-hook! (hack-local-variables-hook :local t)
        (ymacs-python//set-lsp-server)
        (when (and (is-buffer-suitable-for-coding!)
                   (or (eq major-mode 'python-mode)
                       (eq major-mode 'python-ts-mode))
                   (ymacs-lsp//try-enable python))

          (setq ymacs-lsp-format-buffer-function #'ymacs-python/autopep8)
          (setq ymacs-lsp-organize-import-function #'py-isort-buffer)))))

  (define-hook! ymacs-python//inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))
