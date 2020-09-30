;;; -*- lexical-binding: t; -*-

(after! cython-mode
  (define-hook! ymacs-cython|setup (cython-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (local-set-key (kbd "C-c C-b") nil)

    (when (buffer-enable-rich-feature-p)
      (flycheck-mode 1))))

(after! python
  (define-advice python-ffap-module-path (:around (-fn &rest -args) safe)
    (unless (ymacs-python//shell-running-p)
      (apply -fn -args)))

  (define-hook! ymacs-python|setup (python-mode-hook)
    (setq electric-indent-chars (delq ?: electric-indent-chars))

    (when (file-remote-p default-directory)
      (setq-local python-shell-interpreter "python3")
      (setq-local python-shell-interpreter-args "-i"))

    (when (and (buffer-enable-rich-feature-p)
               (eq major-mode 'python-mode))
      (semantic-idle-summary-mode -1)
      (ymacs-lsp//try-enable python
        :fallback
        (progn
          (elpy-mode 1)
          (flycheck-mode -1)
          (ymacs-company//add-backend 'elpy-company-backend)))))

  (define-hook! ymacs-python|inferior-setup (inferior-python-mode-hook)
    (remove-hook 'comint-output-filter-functions
                 #'python-pdbtrack-comint-output-filter-function)))
