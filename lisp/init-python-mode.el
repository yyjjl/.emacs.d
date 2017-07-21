(defhook python|setup (python-mode-hook)
  (local-set-key (kbd "C-c b") 'elpy-autopep8-fix-code)
  (local-set-key (kbd "C-c t t") 'python|pytest-file)
  (local-set-key (kbd "C-c t d") 'python|pytest-directory)
  (local-set-key (kbd "C-c B") 'py-isort-buffer)
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (unless (buffer-temporary-p)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (semantic-idle-summary-mode -1)
    (elpy-mode 1)))

(with-eval-after-load 'py-isort
  (setq py-isort-options '("--lines=100")))

(with-eval-after-load 'elpy
  (remap-keybindings "C-c C-r" "C-c r" elpy-mode-map)
  (setcar elpy-test-discover-runner-command "python3")
  (setq elpy-rpc-backend "jedi"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        elpy-rpc-python-command "python3"
        elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (define-keys :map elpy-mode-map
    ("C-c C-n" . nil)
    ("C-c C-p" . nil)))


;; Pytest tools

(defvar python|pytest-executable "pytest")
(defvar python|pytest-cmd-flags "")
(defconst python|pytest-cmd-fmt "%s %s %s\n")

(defun python|pytest-run (arg)
  (if python|has-pytest-p
      (let ((pytest-buffer (term|local-shell)))
        (shell pytest-buffer)
        (process-send-string (get-buffer-process pytest-buffer)
                             (format python|pytest-cmd-fmt
                                     python|pytest-executable
                                     python|pytest-cmd-flags
                                     arg)))
    (message "'pytest' is not found !!!")))

(defun python|pytest-file ()
  (interactive)
  (let ((file (ivy-read "File name: " #'read-file-name-internal
                        :matcher #'counsel--find-file-matcher)))
    (python|pytest-run file)))

(defun python|pytest-directory ()
  (interactive)
  (let ((file (ivy-read "Directory name: " #'read-file-name-internal
                        :matcher #'counsel--find-file-matcher)))
    (python|pytest-run (file-name-directory file))))

(defun python|get-class-defs ()
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
  (setq python-shell-prompt-detect-failure-warning nil)
  (elpy-enable)
  (remove-hook 'python-mode-hook 'elpy-mode))

(provide 'init-python-mode)
