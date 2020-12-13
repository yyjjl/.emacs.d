;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! flycheck
    (define-advice flycheck-get-next-checker-for-buffer (:around (-fn -checker) fix)
      "When using pyright language server, python-flake8 should also be activated."
      (if (and (bound-and-true-p lsp-mode)
               (eq ymacs-python-lsp-server 'pyright)
               (derived-mode-p 'python-mode))
          (when (eq -checker 'lsp)
            'python-flake8)
        (funcall -fn -checker)))))

(after! cython-mode
  (define-hook! ymacs-cython|setup (cython-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (local-set-key (kbd "C-c C-b") nil)

    (when (buffer-enable-rich-feature-p)
      (flycheck-mode 1))))

(after! python
  (define-hook! ymacs-python|setup-compilation (comint-exec-hook)
    (when (bound-and-true-p compilation-shell-minor-mode)
      (when (cl-some (lambda (str)
                       (let ((exe (car (split-string str))))
                         (and (string-match-p "python" exe)
                              (executable-find exe))))
                     (process-command (get-buffer-process (current-buffer))))
        (setq truncate-lines nil)
        (ymacs-python//enable-pdbtrack))))

  (define-hook! ymacs-python|setup (python-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (when (file-remote-p default-directory)
      (setq-local python-shell-interpreter "python3")
      (setq-local python-shell-interpreter-args "-i"))

    (when (and (buffer-enable-rich-feature-p)
               (eq major-mode 'python-mode))

      (eval-when-has-feature! lsp
        (when (eq ymacs-python-lsp-server 'pyright)
          (require 'lsp-pyright)))

      (try-enable-lsp! python)))

  (define-hook! ymacs-python|inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))
