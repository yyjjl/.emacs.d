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

(provide 'init-python-mode)