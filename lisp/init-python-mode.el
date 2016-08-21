(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (elpy-enable)
    (elpy-mode 1)
    (semantic-mode 1)
    (local-set-key (kbd "<backtab>") 'elpy-autopep8-fix-code)
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))

(setq interpreter-mode-alist
      (cons '("python" .   python-mode) interpreter-mode-alist))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(provide 'init-python-mode)
