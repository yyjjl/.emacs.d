(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (try-turn-on-semantic-mode)
    (semantic-idle-summary-mode -1)
    ;; (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
    (local-set-key (kbd "C-c b") 'yapfify-buffer)
    (local-set-key (kbd "C-c B") 'py-isort-buffer)
    (local-set-key (kbd "<backtab>") 'company-complete)
    (local-set-key (kbd "C-c t t") 'pytest-one)
    (local-set-key (kbd "C-c t d") 'pytest-directory)
    (anaconda-mode 1)
    (anaconda-eldoc-mode 1)
    (push 'company-anaconda company-backends)
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'anaconda-mode
  (setq python-shell-interpreter "python3"))


(defvar pytest-executable "pytest")
(defvar pytest-cmd-flags "")
(defconst pytest-cmd-fmt "%s %s %s\n")

(defun pytest-run (arg)
  (let ((pytest-buffer (get-term)))
    (shell pytest-buffer)
    (process-send-string (get-buffer-process pytest-buffer)
                         (format pytest-cmd-fmt
                                 pytest-executable
                                 pytest-cmd-flags
                                 arg))))

(defun pytest-one ()
  (interactive)
  (pytest-run (read-string "pytest-file:"
                           (file-name-nondirectory (buffer-file-name)))))

(defun pytest-directory ()
  (interactive)
  (pytest-run (read-string "pytest-directory:")))

(provide 'init-python-mode)