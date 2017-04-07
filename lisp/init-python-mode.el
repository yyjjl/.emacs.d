(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (try-turn-on-semantic-mode)
    (semantic-idle-summary-mode -1)
    (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
    (local-set-key (kbd "<backtab>") 'company-complete)
    (local-set-key (kbd "C-c t t") 'pytest-one)
    (local-set-key (kbd "C-c t d") 'pytest-directory)
    (local-set-key (kbd "C-c B") 'py-isort-buffer)
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'elpy
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (bind-keys :map elpy-mode-map
             ("C-c C-n" . nil)
             ("C-c C-p" . nil)))
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