(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook (lambda ()
                               (turn-on-stripe-buffer-mode)
                               (dired-omit-mode 1)))
  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (define-key dired-mode-map ")" 'dired-omit-mode)

  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  (require 'dired-x)
  (require 'dired+)
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

(with-eval-after-load 'dired+
  (diredp-toggle-find-file-reuse-dir 1))


(provide 'init-dired)

