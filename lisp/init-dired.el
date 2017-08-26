;; Improve `dired-mode'
(require! 'dired+)



(defun dired/git-command-in-shell ($command &optional $arg $file-list)
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
  (unless (string-match "[*?][ \t]*\\'" $command)
    (setq $command (concat $command " *")))
  (setq $command (concat "git " $command))
  (dired-do-shell-command $command $arg $file-list)
  (message $command))

(defun dired/open-externally ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (dired-get-marked-files)
                      (list (buffer-file-name))))
         (do-it-p (or (<= (length file-list) 5)
                      (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (cond
       ((eq system-type 'darwin)
        (mapc (lambda (path)
                (shell-command (concat "open " (shell-quote-argument path))))
              file-list))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (path) (let ((process-connection-type nil))
                               (start-process "" nil "xdg-open" path)))
              file-list))))))

(with-eval-after-load 'dired
  (define-hook! dired|setup (dired-mode-hook)
    (dired-hide-details-mode 1))

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq wdired-allow-to-change-permissions t)

  (define-key dired-mode-map ")" 'dired-omit-mode)
  (define-key dired-mode-map "E" 'dired/open-externally)

  (require 'dired-x)
  (require 'dired+)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

(with-eval-after-load 'dired+
  (diredp-toggle-find-file-reuse-dir 1))


(provide 'init-dired)
