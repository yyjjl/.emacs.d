(defun python-mode-hook-setup ()
  (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
  (local-set-key (kbd "C-c t t") 'pytest-one)
  (local-set-key (kbd "C-c t d") 'pytest-directory)
  (local-set-key (kbd "C-c B") 'py-isort-buffer)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (elpy-mode 1)
    ;; use highlight-indent-guides-mode instead
    (highlight-indentation-mode -1)
    (semantic-idle-summary-mode -1)))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'elpy
  (remap-kbd "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (define-keys :map elpy-mode-map
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

(defun python-get-class-defs ()
  (interactive)
  (let* ((indent (make-string 4 (string-to-char " "))))
    (save-excursion
      (forward-line 1)
      (if (re-search-backward "^\\( *\\)class.+\n")
          (progn
            (goto-char (match-end 0))
            (let* ((extra-indent (match-string 1))
                   (re (format "^%s%sdef +\\([^(]+\\)" extra-indent indent))
                   bound defs)
              (when (re-search-forward
                     (format "\\(^%s%s\\(.+\\)\\|\n\\)*" extra-indent indent)
                     nil nil)
                (setq bound (match-end 0))
                (goto-char (match-beginning 0))
                (while (re-search-forward re bound t)
                  (add-to-list 'defs (match-string 1))))
              (kill-new (concat (string-join defs " ")))
              (message "copy => all defs")))
        (message "Can not find where is the class")))))

(with-eval-after-load 'python
  (setq python-shell-prompt-detect-failure-warning nil))

(provide 'init-python-mode)