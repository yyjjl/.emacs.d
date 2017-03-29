(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (try-turn-on-semantic-mode)
    (semantic-idle-summary-mode -1)
    (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
    (local-set-key (kbd "C-c B") 'py-isort-buffer)
    (local-set-key (kbd "<backtab>") 'company-complete)
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'elpy
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python3"
        python-shell-interpreter "python3"
        elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (bind-keys :map elpy-mode-map
             ("C-c C-n" . nil)
             ("C-c C-p" . nil)))

(provide 'init-python-mode)